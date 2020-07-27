//
// Created by jwscoggins on 7/26/20.
//

#include "VoidAtom.h"
#include "Environment.h"
namespace maya {
    Void::Void(Environment &parent) : Atom(parent, VOID_TYPE) { }
} // end namespace maya
