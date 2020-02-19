// maya
// Copyright (c) 2012-2018, Joshua Scoggins 
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in the
//       documentation and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR 
// ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

// header for all the different maya extensions
#ifndef __MAYA_SETUP_H__
#define __MAYA_SETUP_H__

#ifndef MAYA_EXTENSIONS
#define MAYA_EXTENSIONS 1
#endif

#if !MAYA_EXTENSIONS
#define BOOST_EXTENSIONS 0
#define FUNCTIONAL_EXTENSIONS 0
#define TAGLIB_EXTENSIONS 0
#endif

#ifdef PLATFORM_ARDUINO
#define BOOST_EXTENSIONS 0
#define TAGLIB_EXTENSIONS 0
#endif

// should we enable the boost library extensions (header only!)
#ifndef BOOST_EXTENSIONS
#define BOOST_EXTENSIONS 1
#endif

// should we enable the functional programming extensions?
#ifndef FUNCTIONAL_EXTENSIONS
#define FUNCTIONAL_EXTENSIONS 1
#endif

// Should we enable the interface with taglib?
#ifndef TAGLIB_EXTENSIONS
#define TAGLIB_EXTENSIONS 1
#endif // end TAGLIB_EXTENSIONS


#endif // end __MAYA_SETUP_H__

