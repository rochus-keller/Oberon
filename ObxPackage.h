#ifndef OBXPACKAGE_H
#define OBXPACKAGE_H

/*
* Copyright 2021 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Oberon+ parser/compiler library.
*
* The following is the license that applies to this copy of the
* library. For a license to use the library under conditions
* other than those described here, please email to me@rochus-keller.ch.
*
* GNU General Public License Usage
* This file may be used under the terms of the GNU General Public
* License (GPL) versions 2.0 or 3.0 as published by the Free Software
* Foundation and appearing in the file LICENSE.GPL included in
* the packaging of this file. Please review the following information
* to ensure GNU General Public Licensing requirements will be met:
* http://www.fsf.org/licensing/licenses/info/GPLv2.html and
* http://www.gnu.org/copyleft/gpl.html.
*/

#include <QByteArrayList>
#include <QStringList>

namespace Obx
{
    typedef QByteArrayList VirtualPath;
    // A virtual path is a hierarchical name to identify a Package or a Module
    // either the complete path of a virtual dir or a module in it

    // A Package is just a bunch of physical module files which are associated with the same VirtualPath
    struct Package
    {
        VirtualPath d_path;
        QStringList d_files; // list of absolute module file paths belonging to the package
    };
    typedef QList<Package> PackageList;

}

#endif // OBXPACKAGE_H
