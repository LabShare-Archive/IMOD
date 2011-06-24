/******************************************************************************
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
** FILENAME:    svlOptions.h
** AUTHOR(S):   Stephen Gould <sgould@stanford.edu>
** DESCRIPTION:
**   Defines an abstract interface for setting arbitrary options used by
**   other classes. The class provides lots of error checking to make running
**   experiments more robust. Reading settings is a slow process so do not
**   use the getOption methods within a tight loop--rather save the option
**   value in a variable (either in the optionChanged method or before
**   entering the loop).
**
*****************************************************************************/

#pragma once

#include <string>
#include <string.h>
#include <vector>
#include <map>

#include "xmlParser/xmlParser.h"

using namespace std;

class svlOptions {
 private:
    // Options. This data member is private, it cannot be accessed directly
    // from derived classes. Instead derived classes should use the access
    // functions: declareOption, setOption and getOption. All options need
    // to be declared before they can be set. This is usually done in the
    // derived class's constructor.
    mutable map<string, string> _options;

 public:
    svlOptions() { /* do nothing */ }
    virtual ~svlOptions() { /* do nothing */ }

    // Called by owner of the derived class to set options.
    void setOption(const string& name, const string& value);
    void setOption(const string& name, const char *value);
    void setOption(const string& name, int value);
    void setOption(const string& name, double value);
    void setOption(const string& name, bool value);
    void setOptions(const map<string, string>& options);
    void setOptionsFromString(const string& options);
    void setOptionsFromXML(XMLNode& root, const char *tag = "Option");

    // Used to access option settings.
    const string& getOption(const string& name) const;
    bool getOptionAsBool(const string& name) const;
    int getOptionAsInt(const string& name) const;
    double getOptionAsDouble(const string& name) const;

    // Return list of all option names.
    vector<string> getOptionNames() const;

 protected:
    // Declare options in the constructor for the class. An undeclare
    // method is provided to be able to remove options in derived classes.
    void declareOption(const string& name, const string& defaultValue);
    void declareOption(const string& name, const char *defaultValue = NULL);
    void declareOption(const string& name, int defaultValue);
    void declareOption(const string& name, double defaultValue);
    void declareOption(const string& name, bool defaultValue);
    void undeclareOption(const string& name);

    // Called whenever an option changes (i.e. setOption or setOptions
    // is called). Should be overridden by derived classed. Can be used to
    // validate options or setup internal datastructures. The default
    // behaviour is to do nothing.
    virtual void optionChanged(const string& name, const string& value) {
	// do nothing
    }
};


