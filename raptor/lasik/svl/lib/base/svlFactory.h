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
** FILENAME:    svlFactory.h
** AUTHOR(S):   Ian Goodfellow <ia3n@cs.stanford.edu>
**              
** DESCRIPTION:
**  Allows construction of objects in cases where the type is not known
**  until runtime, either by id or from a file.
**
*****************************************************************************/

#pragma once

#include <map>
#include <string>
#include <vector>
#include "svlLogger.h"
using namespace std;

//Functions that examine a file and return "" if the type is not
//recognized as a member of the family or return a string identifying
//the type of object from the family that is in the file
typedef string (*svlFileChecker)(const char *);


template <typename C>
C* svlMakeObject()
{
  return new C();
}

template <typename T>
class svlFactory
{
 public:

 //Functions that create a specific member of the object family
  typedef T * (*T_creator)(void);

  svlFactory(const string & familyName);

  class autoreg
  {
  public:
    autoreg(const char * id,  T_creator func , svlFactory<T> & base)
      {
	base.registerType( string(id), func);
      }
    
    autoreg(svlFileChecker checker, svlFactory<T> & base)
      {
	base.registerFileChecker(checker);
      }


private:
	autoreg() {}
  };


  T * make(const string & id);
  T * load(const char * filename);
  T * load(XMLNode & node);
  void registerFileChecker(svlFileChecker checker);
  void registerType(const string & id, T_creator creator);

 protected:

  string defaultFileChecker(XMLNode & root);
  
  string _familyName;
  map<string, T_creator> _registry;
  vector<svlFileChecker> _fileCheckers;

};


template <typename T>
svlFactory<T>::svlFactory(const string & familyName) : _familyName(familyName)
{
}

template <typename T>
T * svlFactory<T>::make(const string & type)
{
  typename map<string, T_creator>::iterator result = _registry.find(type);
  if (result == _registry.end())
    {
      SVL_LOG(SVL_LOG_WARNING, "svlFactory<"<<_familyName<<"> asked to make object of unknown type \""<<type<<"\"");
      return NULL;
    }
  return result->second();
}

template <typename T>
T * svlFactory<T>::load(const char * filename)
{
  ifstream ifs;
  ifs.open(filename);
  if (ifs.fail())
    {
      SVL_LOG(SVL_LOG_WARNING, "svlFactory"<<_familyName<<">::load: File \""<<filename<<"\" does not exist.");
      return NULL;
    }
  ifs.close();

  XMLNode root = XMLNode::parseFile(filename);

  string xmlResult = defaultFileChecker(root);

  if (xmlResult != "")
    {
      T * rval = make(xmlResult);
      if (rval)
	{
	  if (rval->load(root))
	    {
	      return rval;
	    }
	  else
	    {
	      delete rval;
	      return NULL;
	    }
	}
      
      SVL_LOG(SVL_LOG_WARNING, "svlFactory<"<<_familyName<<">::load: Identified file \""<<filename<<"\" as being of type \""<<xmlResult<<"\", but could not allocate an object of that type.");
      return NULL;
    }
  
  for (unsigned i = 0; i < _fileCheckers.size(); i++)
    {
      string result = _fileCheckers[i](filename);
      if (result != "")
	{
	  T * rval = make(result);
	  if (rval)
	    {
	      if (rval->load(filename))
		{
		  return rval;
		}
	      else
		{
		  delete rval;
		  return NULL;
		}
	    }
	  else
	    {
	      SVL_LOG(SVL_LOG_WARNING, "svlFactory<"<<_familyName<<">::load: Identified file \""<<filename<<"\" as being of type \""<<result<<"\", but could not allocate an object of that type.");
	      return NULL;
	    }
	}
    }
  
  SVL_LOG(SVL_LOG_WARNING, "svlFactory<"<<_familyName<<">::load: No registered file checking function could identify the contents of file \""<<filename<<"\"");

  return NULL;
}

template <typename T>
T * svlFactory<T>::load(XMLNode & node)
{
  string xmlResult = defaultFileChecker(node);
  
  if (xmlResult != "")
    {
      T * rval = make(xmlResult);
      if (rval)
	{
	  if (rval->load(node))
	    {
	      return rval;
	    }
	  else
	    {
	      delete rval;
	      return NULL;
	    }
	}
    }
 
  return NULL;
}

template <typename T>
void svlFactory<T>::registerFileChecker(svlFileChecker checker)
{

  for (unsigned i = 0; i < _fileCheckers.size(); i++)
    if (_fileCheckers[i] == checker)
      {
	SVL_LOG(SVL_LOG_WARNING, "svlFactory<"<<_familyName<<">::registerTypeFileChecker: prevented attempt to register the same checker twice");
	return;
      }

  _fileCheckers.push_back(checker);
}

template <typename T>
void svlFactory<T>::registerType(const string & id, T_creator creator)
{
  if (_registry.find(id) != _registry.end())
    {
      SVL_LOG(SVL_LOG_WARNING, "svlFactory<"<<_familyName<<"::registerType: type \""<<id<<"\" registered twice.");
    }

  SVL_LOG(SVL_LOG_VERBOSE, "svlFactory<"<<_familyName<<"::registerType: type \""<<id<<"\" registered.");

  _registry.insert(pair<string, T_creator>(string(id), creator));
}

template <typename T>
string svlFactory<T>::defaultFileChecker(XMLNode & root)
{
   //This function should not display error messages because it could just
  //be that it is not the right file checker being called. The callee
  //should already have checked that the file exists, so we don't need
  //to report that either (though none of this code should crash if
  //the file does not exist)
  
  //An empty root probably indicates the file doesn't exist or something like that
  if (root.isEmpty())
    return "";
  
  if (!root.getName())
    {
      //Sometimes the root has no name, even though the file is valid. In
      //these cases it has a single child, which is the real root
      
      if (root.nChildNode() != 1)
	return "";

      root = root.getChildNode(0);
      
      if (!root.getName())
	return "";
    }
  
  if (!strcmp(root.getName(),_familyName.c_str()))
    {
      return string(root.getAttribute("id"));
    }

  return "";
}

//Put this macro in the header of a class to be auto-registered
//For instance, to autoregister svlBoostedClassifier to have id
//CVBOOST and be  loaded by svlClassifierFactory,
//use SVL_AUTOREGISTER_H( CVBOOST, svlClassifier )
//You must also put a macro in the cpp file
#define SVL_AUTOREGISTER_H( id , objectType ) \
  extern svlFactory<objectType>::autoreg svlAutoreg ## id ## objectType;

//Put this macro in the cpp file of a class to be auto-registered
//For instance, to autogregister CVBOOST to have id CVBOOST and be
//loaded by svlClassifierFactory, use 
// SVL_AUTOREGISTER_CPP( CVBOOST , svlClassifier, svlBoostedClassifier )
#define SVL_AUTOREGISTER_CPP( id, objectType, subclass ) \
  objectType * svlAutoregMake ## subclass () \
  {					       \
    return new subclass ();		       \
  }					       \
					       \
  svlFactory<objectType>::autoreg svlAutoreg ## id ## objectType \
  ( #id , & svlAutoregMake ## subclass , objectType ## Factory());


//Similar macros for registering file checkers
#define SVL_AUTOREGISTER_FILECHECKER_H( fileChecker, objectType) \
  extern svlFactory<objectType>::autoreg svlAutoreg ## fileChecker ## objectType;

#define SVL_AUTOREGISTER_FILECHECKER_CPP( fileChecker, objectType) \
  svlFactory<objectType>::autoreg svlAutoreg ## fileChecker ## objectType \
  ( & fileChecker, objectType ## Factory());
