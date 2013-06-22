/*
 *  sourcedoc.cpp -- Program to make html documents from source files
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 */                                                                           

#include <stdlib.h>
#include <stdio.h>
#include <qstring.h>
#include <qstringlist.h>
#include <qfile.h>
#include <qdir.h>
#include <qregexp.h>
#include <QTextStream>
#include "b3dutil.h"

#define LATIN1(a) ((const char *)a.toLatin1())

#define CAPTURE_NONBS "([^\\\\])"

static void convertSpecialCodes(QString &str, char *progname, int debug);

void usage(char *progname) 
{
  printf("Usage: %s [options] input_doc output_doc\n",
         progname);
  printf("  input_doc is the input html document to be scanned\n");
  printf("  output_doc is the output document\n");
  printf(" Options:\n");
  printf("    -f       Source files are Fortran (default is C/C++)\n");
  printf("    -d path  Set path to source files\n");
  printf("    -D       Debug mode\n");
  exit(1);
}

int main(int argc, char *argv[])
{
  char *listString = "LIST FUNCTIONS FROM ";
  char *descString = "DESCRIBE FUNCTIONS FROM ";
  char *listCode = "LIST CODE FROM ";
  char *descCode = "DESCRIBE CODE FROM ";
  char *secString = "DOC_SECTION";
  char *endSecString = "END_SECTION";
  char *codeString = "DOC_CODE";
  char *endCodeString = "END_CODE";
  char *noFunc = "0";
  QString str, str2, fname, href, funcName, secName, path;
  QStringList descList;
  QStringList fnameList;
  QStringList sectionList, tmpList;
  int fileStart[10000];
  int ind, ind1, ind2, i;
  unsigned int ui;
  char *progname = "sourcedoc";
  bool fort77 = false;
  bool doCode;
  bool codeOut;
  QString docStart = "/\\*!";
  QString docEnd = "\\*/";
  QString docContinue = "\\*";
  QString nonDocComment = "^[ \t]*/[/\\*]";
  QString codeComment = "^[ \t]*/\\*";
  int debug = 0;
  bool inComment, inSection;

  if (argc < 3) {
    // It needs to run without libraries
    /* imodVersion(progname);
       imodCopyright(); */
    usage(progname);
  }

  // Parse the meager argument list
  ind = 1;
  while (argc - ind > 2) {
    if (argv[ind][0] == '-'){
      switch (argv[ind][1]){
      case 'f':
        fort77 = true;
        docStart = "^[Cc!][ \t]*!";
        docEnd = docStart;
        docContinue = "[cC!]";
        nonDocComment = "^[Cc!]";
        break;

      case 'D':
        debug = 1;
        break;

      case 'd':
        path = QString(argv[++ind]) + QDir::separator();
        break;

      default:
        fprintf(stderr, "ERROR: %s - unknown argument %s\n", progname, 
                argv[ind]);
        usage(progname);
      }
    } else {
      fprintf(stderr, "ERROR: %s - too many arguments\n", progname);
      usage(progname);
    }
    ind++;
  }
  
  str = argv[ind];
  QFile inFile(str);
  if (!inFile.open(QIODevice::ReadOnly | QIODevice::Text)) {
    fprintf(stderr, "ERROR: %s - cannot open input file %s\n", progname, 
            argv[ind]);
    exit(1);
  }

  // Don't want to make backup files anyway!
  /*if (imodBackupFile(argv[ind + 1])) {
    fprintf(stderr, "ERROR: %s - could not create backup file", progname);
    exit(3);
    }*/

  str = argv[ind + 1];
  QFile outFile(str);
  if (!outFile.open(QIODevice::WriteOnly | QIODevice::Text)) {
    fprintf(stderr, "ERROR: %s - cannot open output file %s\n", progname,
            argv[ind + 1]);
    exit(1);
  }

  QTextStream inStream(&inFile);
  QTextStream outStream(&outFile);
  fileStart[0] = 0;

  while (!inStream.atEnd()) {
    
    // Read lines and look for list or describe strings
    str = inStream.readLine();
    doCode = false;
    ind1 = str.indexOf(listString);
    ind2 = str.indexOf(descString);
    if (ind1 < 0 && ind2 < 0) {
      ind1 = str.indexOf(listCode);
      ind2 = str.indexOf(descCode);
      doCode = ind1 >= 0 || ind2 >= 0;
    }
    if (ind1 >= 0) {

      // get file name and section name and open the file
      fname = str.right(str.length() - ind1 - 
                        (doCode ? strlen(listCode) : strlen(listString)));
      fname = fname.trimmed();
      if (fname.startsWith("\"")) {
        tmpList = fname.split('\"', QString::SkipEmptyParts);
        if (tmpList.count() > 1)
          tmpList[1].trimmed();
      } else {
        tmpList = fname.split(' ', QString::SkipEmptyParts);
      }
      fname = tmpList[0];
      secName = "";
      if (tmpList.count() > 1)
        secName = tmpList[1];
      
      if (debug)
        puts(LATIN1(fname));
      if (debug)
        puts(LATIN1(secName));

      if(debug)
        puts((const char *)((path + fname).toLatin1()));
      QFile srcFile(path + fname);
      if (!srcFile.open(QIODevice::ReadOnly | QIODevice::Text)) {
        fprintf(stderr, "ERROR: %s - cannot open source file %s\n", progname, 
                LATIN1(fname));
        exit(1);
      }
      QTextStream srcStream(&srcFile);
      inSection = false;

      // Scan through file looking for start sequence
      while (!srcStream.atEnd()) {
        str = srcStream.readLine();

        // Is it a section sequence?
        // If not looking for sections, set in section flag unconditionally
        // Set flag if this is section of choice and go to next line
        if (str.contains(secString)) {
          inSection = secName.isEmpty() || str.contains(secName);
          continue;
        }

        // If it is an end section string, clear flag and continue
        if (str.contains(endSecString)) {
          inSection = false;
          continue;
        }

        // If not in a chosen section, or in a section when not looking for
        // one, skip line
        if(!secName.isEmpty() && !inSection || secName.isEmpty() && inSection) 
          continue;

        if (!doCode && str.contains(QRegExp(docStart))) {

          // Found start sequence.  Start a string list for doc, strip the
          // start sequence, and add string to list if anything remains
          QStringList docList;
          str = str.trimmed();
          ind = str.indexOf("!");
          str = str.right(str.length() - ind - 1);
          if (debug)
            puts(LATIN1(str));

          // Loop on body of doc copying to list until end sequence
          while (1) {
            ind = str.indexOf(QRegExp(docEnd));
            if (ind >= 0) {

              // Found end sequence; strip it and add to list if not empty
              if (fort77) {
                ind = str.indexOf("!");
                str = str.right(str.length() - ind - 1);
              } else {
                str.truncate(ind);
              }
              str = str.trimmed();
              if (!str.isEmpty())
                docList << str;
              break;
            }

            // Add current string to list if not empty or list already started
            if (docList.count() || !str.isEmpty())
              docList << str;

            // Get new string and loop
            str = srcStream.readLine();
            if (debug)
              puts(LATIN1(str));
            if (str.isNull()) {
              fprintf(stderr, "ERROR: %s - end of file %s in middle of "
                      "comment\n", progname, LATIN1(fname));
              exit(1);
            }
          }

          // Now get function sequence: start list and save lines until one
          // ends in )  Omit leading blank lines if any
          QStringList funcList;
          inComment = false;
          do {
            str = srcStream.readLine();
            if (debug)
              puts(LATIN1(str));
            if (str.isNull()) {
              fprintf(stderr, "ERROR: %s - end of file %s in middle of "
                      "function\n", progname, LATIN1(fname));
              exit(1);
            }

            // If no function yet, skip if find a non-documentation comment
            // or if we are continuing one
            if (!funcList.count() && 
                (inComment || str.indexOf(QRegExp(nonDocComment)) >= 0)) {
              
              // In C, if in a comment or comment contains /*, set flag
              // for continuation if we don't find */
              if (!fort77 && (inComment || str.indexOf("/*") >= 0))
                inComment = str.indexOf("*/") < 0;

              // clear string and skip the line
              str = "";
              continue;
            }

            str = str.trimmed();
            ind = str.indexOf(QRegExp("\\) *\\{"));
            if (ind > 0)
              str.truncate(ind + 1);
            ind = str.indexOf(QRegExp("\\) *;"));
            if (ind > 0)
              str.truncate(ind + 1);

            // Fort77 continuation: strip it off
            if (fort77 && funcList.count()) {
              str = str.right(str.length() - 1);
              str = str.trimmed();
            }

            if (!str.isEmpty() || funcList.count())
              funcList << str;
          } while (!str.endsWith(")"));

          // Extract the function name from first line
          str = funcList[0];
          if (debug)
            puts(LATIN1(str));
          ind2 = str.indexOf(QRegExp(" *\\("));
          if (debug)
            printf("ind2 %d\n", ind2);
          ind1 = str.lastIndexOf(QRegExp("[* :]"), ind2 - 1) + 1;
          funcName = str.mid(ind1, ind2 - ind1);

          if (debug)
            puts(LATIN1(funcName));
            
          // Compose entry and output to the list 
          if (funcName != QString(noFunc)) {
            str2 = "<BR>";
            if (ind1)
              str2 += str.left(ind1);
            str2 += "<A HREF=\"#" + funcName + "\">" + funcName + "</A>" + 
              (str.right(str.length() - ind2)).trimmed();
            outStream << str2 << "\n";
            for (ui = 1; ui < funcList.count(); ui++)
              outStream << funcList[ui] << "\n";

            // Add function string to the description list
            
            for (ui = 0; ui < funcList.count(); ui++) {
              str2 = "";
              if (!ui)
                str2 = "<H3><A NAME=\"" + funcName + "\"></A>";
              str2 += funcList[ui];
              if (ui == funcList.count() - 1)
                str2 += "</H3><P>";
              descList << str2;
            }
          } else {

            // If there is no function, just output paragraph to desc list
            descList << QString("<P>\n");
          }

          if (debug)
            puts("Adding docs to desc");

          // Edit each doc line and add it to description list
          for (ui = 0; ui < docList.count(); ui++) {
            str = docList[ui].trimmed();
            while (!str.indexOf(QRegExp(docContinue)))
              str = (str.right(str.length() - 1)).trimmed();
            convertSpecialCodes(str, progname, debug);
            descList << str;
          }
          descList << QString("</P>\n");

        } else if (doCode && str.contains(codeString)) {

          // Found a CODE start, next get the name as the remainder of line
          ind = str.indexOf(codeString);
          funcName = str.right(str.length() - ind - strlen(codeString));
          ind = funcName.indexOf(QRegExp(docEnd));
          if (ind >= 0)
            funcName.truncate(ind);
          funcName = funcName.trimmed();
          if (debug)
            puts(LATIN1(funcName));

          // Output the list entry and add to description list
          str2 = "<BR><A HREF=\"#" + funcName + "\">" + funcName + "</A>\n";
          outStream << str2;
          str2 = "<H3><A NAME=\"" + funcName + "\"></A>" + funcName + "</H3>";
          descList << str2;

          // Loop on the lines until the end code
          inComment = false;
          codeOut = false;
          while (1) {
            str = srcStream.readLine();
            if (debug)
              puts(LATIN1(str));
            if (str.isNull()) {
              fprintf(stderr, "ERROR: %s - end of file %s in middle of "
                      "code\n", progname, LATIN1(fname));
              exit(1);
            }
            
            if (str.contains(endCodeString))
              break;

            // Test for comment first time in 
            if (!codeOut && !inComment && str.contains(QRegExp(codeComment)))
              inComment = true;
            if (inComment) {


              // If in comment, first test
              // if comment end is present, remove it and clear comment flag
              ind = str.indexOf(QRegExp(docEnd));
              if (ind >= 0) {
                inComment = false;
                str.truncate(ind);
              }

              // remove up to the starting *
              ind = str.indexOf('*');
              if (ind >= 0)
                str = str.right(str.length() - ind - 1);

              // Strip the string, interpret codes, and put it out if not empty
              str = str.trimmed();
              if (!str.isEmpty()) {
                convertSpecialCodes(str, progname, debug);
                descList << str;
              }

              // For non-comment, just put out string
            } else {
              str.replace("&", "&amp;");
              str.replace("<", "&lt;");
              str.replace(">", "&gt;");
              descList << str;
            }
              
            // Now if not in comment and code not out yet, put it out
            if (!codeOut && !inComment) {
              descList << "<BR><PRE>";
              codeOut = true;
            }
          }

          // Put out terminating CODE if start was put out
          if (codeOut)
            descList << "</PRE>";
              
        }    // End of loop on one function
      }   // End of source file loop

      // Save filename on list, save index to end of strings
      fnameList << fname;
      sectionList << secName;
      fileStart[fnameList.count()] = descList.count();
      srcFile.close();

    } else if (ind2 >= 0) {

      // Descriptions requested: get filename and look up on list; write lines
      fname = str.right(str.length() - ind2 - 
                        (doCode ? strlen(descCode) : strlen(descString)));
      fname = fname.trimmed();
      tmpList = fname.split(' ', QString::SkipEmptyParts);
      fname = tmpList[0];
      secName = "";
      if (tmpList.count() > 1)
        secName = tmpList[1];
      for (ui = 0; ui < fnameList.count(); ui++) {
        if (fname == fnameList[ui] && secName == sectionList[ui]) {
          for (i = fileStart[ui]; i < fileStart[ui + 1]; i++)
            outStream << descList[i] << "\n";
          break;
        }
      }
      if (ui == fnameList.count()) {
        fprintf(stderr, "ERROR: %s - function descriptions requested from "
                "%s but no such\nfile processed for function list\n",
                progname, LATIN1(fname));
        exit(1);
      }

    } else {
      
      // Nothing special, pass the line through
      outStream << str << "\n";
    }
  }  // End of loop on input lines

  inFile.close();
  outFile.close();
  return 0;
}

// Convert all the special codes
static void convertSpecialCodes(QString &str, char *progname, int debug) 
{
  QString str2, funcName, href;
  int ind1, ind2, ind, ind0, ind3;

  str.replace("&", "&amp;");
  str.replace("<", "&lt;");
  str.replace(">", "&gt;");
  str.replace(QRegExp(CAPTURE_NONBS"\\["), "\\1<B>");
  str.replace(QRegExp("^\\["), "<B>");
            
  str.replace(QRegExp(CAPTURE_NONBS"\\]"), "\\1</B>");
  str.replace(QRegExp(CAPTURE_NONBS"\\{"), "\\1<I>");
  str.replace(QRegExp("^\\{"), "<I>");
  str.replace(QRegExp(CAPTURE_NONBS"\\}"), "\\1</I>");
  str.replace(QRegExp(CAPTURE_NONBS"\\^"), "\\1<BR>");
  str.replace(QRegExp("^\\^"), "<BR>");

  // Replace multiple spaces with ;nbsp
  if (str.indexOf("  ") >= 0) {
    str.replace(QRegExp("([^ ]) ([^ ])"), "\\1%_%\\2");
    str.replace(QRegExp("([^ ]) ([^ ])"), "\\1%_%\\2");
    str.replace(" ", "&nbsp;");
    str.replace("%_%", " ");
  }

  if (str.isEmpty())
    str = "<P>";
  if (debug)
    puts(LATIN1(str));

  // Insert a link at an @
  while (str.contains(QRegExp("[^\\\\]@")) || str.startsWith("@")) {

    if (str.startsWith("@"))
      ind0 = 1;
    else
      ind0 = str.indexOf(QRegExp("[^\\\\]@")) + 2;
    ind1 = ind0;

    // If there is a double @, then end is another @
    if (str.at(ind1) == '@') {
      ind1++;
      while (ind1 < (int)str.length() && str.at(ind1) == ' ')
        ind1++;
      ind2 = str.indexOf('@', ind1 + 1);
      if (ind2 < 0) {
        fprintf(stderr, "ERROR: %s - Empty or unterminated @@ link "
                "in\n%s\n", progname, LATIN1(str));
        exit(1);
      }
      ind3 = ind2 + 1;

    } else {
      while (ind1 < (int)str.length() && str.at(ind1) == ' ')
        ind1++;
      ind2 = str.indexOf(QRegExp("[ ,;()]"), ind1);
      if (ind2 < ind1)
        ind2 = str.length();
      ind3 = ind2;
    }
    funcName = str.mid(ind1, ind2 - ind1);
    if (funcName.contains('#')) {
      href = funcName;
      ind = funcName.indexOf('#');
      funcName = funcName.right(funcName.length() - ind - 1);
    } else {
      href = QString("#") + funcName;
    }
    str2 = "";

    // Take string up to the initial @ only
    if (ind0 > 0)
      str2 = str.left(ind0 - 1);
    str2 += "<A HREF=\"" + href + "\">" + funcName + "</A>";
    if (ind3 < (int)str.length())
      str2 += str.right(str.length() - ind3);
    str = str2;
  }

  // For any escaped characters, remove the backslash
  str.replace(QRegExp("\\\\([\\[\\]\\{\\}^@])"), "\\1");
  if (debug)
    puts(LATIN1(str));
}
