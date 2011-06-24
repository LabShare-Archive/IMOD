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
** FILENAME:    svlConfigManager.cpp
** AUTHOR(S):   Stephen Gould <sgould@stanford.edu>
**
*****************************************************************************/

#include <string>
#include <map>

#include "xmlParser/xmlParser.h"

#include "svlBase.h"

using namespace std;

// configuration interface -----------------------------------------------------

svlConfigurableModule::svlConfigurableModule(const char *module) :
    _moduleName(module)
{
    SVL_ASSERT(module != NULL);

    // register the module
    svlConfigurationManager::get().registerModule(this);
}

svlConfigurableModule::~svlConfigurableModule()
{
    // unregister the module
    svlConfigurationManager::get().unregisterModule(this);
}

void svlConfigurableModule::usage(std::ostream &os) const {
    // do nothing (derived classes should override)
}

void svlConfigurableModule::readConfiguration(const char *filename)
{
    SVL_ASSERT(filename != NULL);

    XMLResults parseResults;
    XMLNode root = XMLNode::parseFile(filename, NULL, &parseResults);
    SVL_ASSERT(!root.isEmpty());

    if (parseResults.error != eXMLErrorNone) {
        SVL_LOG(SVL_LOG_FATAL, "error parsing XML file " << filename << "(" << root.getError(parseResults.error) << ")");
    }

    XMLNode node = root.getChildNode(_moduleName.c_str());
    if (node.isEmpty()) {
        SVL_LOG(SVL_LOG_ERROR, "couldn't find configuration for " << _moduleName
            << " in " << filename);
    } else {
        readConfiguration(node);
    }
}

void svlConfigurableModule::readConfiguration(XMLNode& node)
{
    // parse attributes
    for (int i = 0; i < node.nAttribute(); i++) {
        setConfiguration(node.getAttributeName(i), node.getAttributeValue(i));
    }

    // parse <option name="" value=""/>
    for (int i = 0; i < node.nChildNode("option"); i++) {
        XMLNode child = node.getChildNode("option", i);
        const char *name = child.getAttribute("name");
        const char *value = child.getAttribute("value");
        SVL_ASSERT((name != NULL) && (value != NULL));
        setConfiguration(name, value);
    }
}

// library configuration registry ---------------------------------------------

svlConfigurationManager::svlConfigurationManager()
{
    // do nothing
}

svlConfigurationManager::~svlConfigurationManager()
{
    // do nothing
}

svlConfigurationManager& svlConfigurationManager::get()
{
    static svlConfigurationManager mgr;
    return mgr;
}

void svlConfigurationManager::configure(const char *filename)
{
    SVL_ASSERT(filename != NULL); 
    XMLResults parseResults;
    XMLNode root = XMLNode::parseFile(filename, NULL, &parseResults);
    if (root.isEmpty()) {
        SVL_LOG(SVL_LOG_ERROR, "couldn't find configuration in " << filename);
        return;
    }
    if (parseResults.error != eXMLErrorNone) {
        SVL_LOG(SVL_LOG_FATAL, "error parsing XML file " << filename << "(" << root.getError(parseResults.error) << ")");
    }
    configure(root);
}

void svlConfigurationManager::configure(XMLNode& root)
{
    for (int i = 0; i < root.nChildNode(); i++) {
        XMLNode node = root.getChildNode(i);
        svlConfigRegistry::iterator it = _registry.find(string(node.getName()));
        if (it == _registry.end()) {
            SVL_LOG(SVL_LOG_DEBUG, "no module with name " << node.getName() << " has been registered");
        } else {
            SVL_LOG(SVL_LOG_DEBUG, "configuring module " << it->first);
            it->second->readConfiguration(node);
        }
    }
}

void svlConfigurationManager::configure(const char *module, const char *name, const char *value)
{
    SVL_ASSERT((module != NULL) && (name != NULL) && (value != NULL));
    svlConfigRegistry::iterator it = _registry.find(string(module));
    if (it == _registry.end()) {
        SVL_LOG(SVL_LOG_FATAL, "no module with name \"" << module << "\" has been registered");
    } else {
        SVL_LOG(SVL_LOG_DEBUG, "setting " << module << "::" << name << " to " << value);
        it->second->setConfiguration(name, value);
    }
}

void svlConfigurationManager::showRegistry() const
{
    cout << "--- svlConfigurationManager registry ---\n";
    for (svlConfigRegistry::const_iterator it = _registry.begin();
         it != _registry.end(); ++it) {
        cout << "  * " << it->first << "\n";
        it->second->usage(cout);
    }
    cout << "--- -------------------------------- ---\n";
}

void svlConfigurationManager::registerModule(svlConfigurableModule *m)
{
    SVL_ASSERT(m != NULL);

    SVL_LOG(SVL_LOG_DEBUG, "registering module " << m->name());
    _registry[m->name()] = m;
}

void svlConfigurationManager::unregisterModule(svlConfigurableModule *m)
{
    SVL_LOG(SVL_LOG_DEBUG, "unregistering module " << m->name());
    svlConfigRegistry::iterator it = _registry.find(m->name());
    if (it == _registry.end()) {
        SVL_LOG(SVL_LOG_WARNING, "module with name \"" << m->name() 
            << "\" has already been unregistered");
    } else {
        _registry.erase(it);
    }
}


