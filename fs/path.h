/**
 * @file
 * Wrapper around boost's path class
 * @copyright
 * Copyright (c) 2015-2022 Parasoft Corporation
 *
 * This software is provided 'as-is', without any express or implied
 * warranty. In no event will the authors be held liable for any damages
 * arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not
 *    claim that you wrote the original software. If you use this software
 *    in a product, an acknowledgment in the product documentation would be
 *    appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 *
 */
#ifndef _LIBFILESYSTEM_PATH_H_ // {
#define _LIBFILESYSTEM_PATH_H_

#ifdef ARDUINO
#include <filesystem>
#define FilesystemRoot std::filesystem
#else
#include <boost/filesystem.hpp>
#define FilesystemRoot boost::filesystem
#endif
#include <list>
#include <vector>

/**
 * @defgroup fs Filesytem manipulation.
 * Platform independent means of naming files, determining if they exist,
 * renaming them, etc.
 */


/**
 * Encapsulate Filesystem things.
 * @ingroup fs
 */
namespace Neutron
{
    using CopyOption = FilesystemRoot::copy_options;
    using FilesystemRoot::copy_file;
    using FilesystemRoot::copy;
    using FilesystemRoot::remove;
    using Path = FilesystemRoot::path;
    using FilesystemError = FilesystemRoot::filesystem_error;
    using FilesystemRoot::exists;
    using FilesystemRoot::is_regular_file;
    using FilesystemRoot::rename;
    using DirectoryIterator = FilesystemRoot::directory_iterator;
    using FilesystemRoot::is_directory;
    using FilesystemRoot::is_empty;
#ifndef ARDUINO
    using FilesystemRoot::unique_path;
#endif
    using FilesystemRoot::temp_directory_path;
    using FilesystemRoot::absolute;
    using FilesystemRoot::current_path;
    using FilesystemRoot::copy_options;

    using PathVector = std::vector<Path>;
    using PathList = std::list<Path>;

    /// Create a directory given the provided path
    inline bool createDirectory(const Path& p)
    {
        return FilesystemRoot::create_directories(p);
    }

    /// is the given path a regular file?
    inline bool isRegularFile(const Path& p)
    {
        return FilesystemRoot::is_regular_file(p);
    }

    /// is the given path a directory?
    inline bool isDirectory(const Path& p)
    {
        return FilesystemRoot::is_directory(p);
    }

} // namespace Filesystem

#endif // } _LIBFILESYSTEM_PATH_H_

// end of file
