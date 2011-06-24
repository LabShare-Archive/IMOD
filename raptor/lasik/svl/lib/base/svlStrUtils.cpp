/*****************************************************************************
** STAIR VISION LIBRARY
** Copyright (c) 2007-2009, Stephen Gould
** All rights reserved.
**
** Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions are met:
**     * Redistributions of source code must retain the above copyright
**       notice, this list of conditions and the following disclaimer.
**     * Redistributions in binary form must reproduce the above copyright
**       notice, this list of conditions and the following disclaimer in the
**       documentation and/or other materials provided with the distribution.
**     * Neither the name of the Stanford University nor the
**       names of its contributors may be used to endorse or promote products
**       derived from this software without specific prior written permission.
**
** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS ``AS IS'' AND ANY
** EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
** WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
** DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
** DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
** (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
** LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
** ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
** (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
** SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
**
******************************************************************************
** FILENAME:    svlStrUtils.cpp
** AUTHOR(S):   Ian Goodfellow <ia3n@stanford.edu>
**              Stephen Gould <sgould@stanford.edu>
**
*****************************************************************************/

#include <string>
#include <iostream>
#include <sstream>

#include "svlStrUtils.h"

using namespace std;

// toString() routines.
string toString(const map<string, string>& p)
{
    std::stringstream s;    
    for (map<string, string>::const_iterator i = p.begin();
	 i != p.end(); i++) {
	if (i != p.begin()) {
	    s << ", ";
	}
	s << i->first << "=" << i->second;
    }
    return s.str();
}

// Case insensitive comparison
int strNoCaseCompare(const string& A, const string& B)
{
    string::const_iterator itA = A.begin();
    string::const_iterator itB = B.begin();

    while ((itA != A.end()) && (itB != B.end())) { 
        if (::toupper(*itA) != ::toupper(*itB))
            return (::toupper(*itA)  < ::toupper(*itB)) ? -1 : 1; 
        ++itA;
        ++itB;
    }

    if (A.size() == B.size()) 
        return 0;
    return (A.size() < B.size()) ? -1 : 1;
}

// Function to break string of "<name>\s*=\s*<value>[,; ]" pairs
// into an stl map. If the value part does not exist then sets to
// "true".
map<string, string> parseNameValueString(string str)
{
    // first tokenize into <name>=<value> pairs
    vector<string> tokens;
    string::size_type lastPos = str.find_first_not_of(" ", 0);
    string::size_type pos = str.find_first_of(",; ", lastPos);

    while ((string::npos != pos) || (string::npos != lastPos)) {
        // found a token, add it to the vector.
        tokens.push_back(str.substr(lastPos, pos - lastPos));
        // skip delimiters
        lastPos = str.find_first_not_of(",; ", pos);
        // find next "non-delimiter"
        pos = str.find_first_of(",; ", lastPos);
    }

    // now break tokens into name and value pairs
    map<string, string> output;
    for (unsigned i = 0; i < tokens.size(); i++) {
	pos = tokens[i].find_first_of("=", 0);
	if (pos != string::npos) {
	    output[tokens[i].substr(0, pos)] = 
		tokens[i].substr(pos + 1, tokens[i].length() - pos);
	} else {
	    output[tokens[i]] = "true";
	}
    }
   
    return output;
}

string padString(const string& str, int padLength,
                 unsigned char padChar)
{
    if (str.size() >= (unsigned)padLength) {
        return str;
    }

    string padString((unsigned)padLength - str.size(), padChar);
    return (padString + str);
}

string strReplaceSubstr(const string & str, const string & substr, const string & rep)
{
  string rval;

  size_t searchPos = 0, prevPos;

  while (searchPos != string::npos && searchPos < str.size())
    {
      prevPos = searchPos;

      searchPos = str.find(substr, searchPos);
      
      if (searchPos == string::npos)
	rval += str.substr(prevPos);
      else
	{
	  rval += str.substr(prevPos, searchPos - prevPos);
	  rval += rep;

	  searchPos += substr.length();
	}
    }
  return rval;
}

string strBaseName(const string &fullPath)
{
    string baseName;
    
    // strip directory name
    string::size_type pos = fullPath.find_last_of("/\\");
    if (pos == string::npos) {
	baseName = fullPath;
    } else {
	baseName = fullPath.substr(pos + 1, fullPath.length() - pos);
    }

    // strip extension
    return strWithoutExt(baseName);
}

string strFilename(const string &fullPath)
{
    // strip directory name
    string::size_type pos = fullPath.find_last_of("/\\");
    if (pos == string::npos) {
	return fullPath;
    }
    
    return fullPath.substr(pos + 1, fullPath.length() - pos);
}

string strDirectory(const string &fullPath)
{
    string::size_type pos = fullPath.find_last_of("/\\");
    if (pos == string::npos) {
	return string(".");
    }
    
    return fullPath.substr(0, pos);
}

string strExtension(const string &fullPath)
{
    string filename = strFilename(fullPath);
    string::size_type pos = filename.find_last_of(".");
    if (pos != string::npos) {
	return filename.substr(pos + 1, filename.length() - pos);
    }
    
    return string("");
}

string strReplaceExt(const string &fullPath, const string &ext)
{
    string oldExt = strExtension(fullPath);
    int len = oldExt.length() == 0 ? 0 : oldExt.length() + 1;
    return (fullPath.substr(0, fullPath.length() - len) + ext);
}

string strWithoutExt(const string &fullPath)
{
  string filename = fullPath;

  // strip extension
  string::size_type pos = filename.find_last_of(".");
  if (pos != string::npos) {
    filename = filename.substr(0, pos);
  }
  return filename;
}

string strWithoutEndSlashes(const string &fullPath)
{
  string filename = fullPath;
  string::size_type pos = filename.find_last_not_of("/");
  return filename.substr(0, pos+1);
}

// Returns index from filenames with the form <base><index>.<ext>
int strFileIndex(const string &fullPath)
{
    string baseName = strBaseName(fullPath);
    string::size_type ib = baseName.find_first_of("0123456789");
    string::size_type ie = baseName.find_last_of("0123456789");
   
    if ((ib == string::npos) || (ie == string::npos)) {
	return -1;
    }

    string index = baseName.substr(ib, ie - ib + 1);
    return atoi(index.c_str());
}

