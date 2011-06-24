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
** FILENAME:    svlConfigManager.h
** AUTHOR(S):   Stephen Gould <sgould@stanford.edu>
** DESCRIPTION:
**   Module configuration code. Handles configuration of static members in the
**   SVL libraries and applications from XML config files. Also allows projects
**   to register their own configuration code. Configuration is split by library
**   and unit (collectively "module"), e.g. svlML.svlConfusionMatrix.colSep = " & "
**   has module "svlML.svlConfusionMatrix", name "colSep" and value " & ".
**   All identifiers are case-sensitive. Implementers should inherit from
**   svlConfigurableModule and override the usage() and setConfiguration() member
**   functions.
**
*****************************************************************************/

#pragma once

#include <string>
#include <string.h>
#include <iostream>
#include <map>

#include "xmlParser/xmlParser.h"

using namespace std;

// configuration interface ----------------------------------------------------

class svlConfigurableModule {
 private:
    string _moduleName;

 public:
    svlConfigurableModule(const char *module);
    virtual ~svlConfigurableModule();

    inline const string& name() const { return _moduleName; }
    virtual void usage(std::ostream& os) const;

    void readConfiguration(const char *filename);
    virtual void readConfiguration(XMLNode& node);
    virtual void setConfiguration(const char* name, const char *value) = 0;
};

// library configuration registry ---------------------------------------------

class svlConfigurationManager {
 friend class svlConfigurableModule;

 private:
    typedef map<string, svlConfigurableModule *> svlConfigRegistry;
    svlConfigRegistry _registry;

 public:
    ~svlConfigurationManager();

    static svlConfigurationManager& get();

    void configure(const char *filename);
    void configure(XMLNode& root);
    void configure(const char *module, const char *name, const char *value);

    void showRegistry() const;

 protected:
    svlConfigurationManager(); // singleton class so hide constructor

    void registerModule(svlConfigurableModule *m);
    void unregisterModule(svlConfigurableModule *m);
};
