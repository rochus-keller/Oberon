/*
* Copyright 2019 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Oberon parser/code model library.
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

#include <QByteArray>

static inline char at( const QByteArray& str, int i ){
    return ( i >= 0 && i < str.size() ? str[i] : 0 );
}

// Generated with EbnfStudio and manually modified
bool isCppKeyword( const QByteArray& str ) {
    int i = 0;
    bool res = false;
    switch( at(str,i) ){
    case 'a':
        switch( at(str,i+1) ){
        case 'l':
            if( at(str,i+2) == 'i' ){
                if( at(str,i+3) == 'g' ){
                    if( at(str,i+4) == 'n' ){
                        switch( at(str,i+5) ){
                        case 'a':
                            if( at(str,i+6) == 's' ){
                                res = true; i += 7;
                            }
                            break;
                        case 'o':
                            if( at(str,i+6) == 'f' ){
                                res = true; i += 7;
                            }
                            break;
                        }
                    }
                }
            }
            break;
        case 'n':
            if( at(str,i+2) == 'd' ){
                if( at(str,i+3) == '_' ){
                    if( at(str,i+4) == 'e' ){
                        if( at(str,i+5) == 'q' ){
                            res = true; i += 6;
                        }
                    }
                } else {
                    res = true; i += 3;
                }
            }
            break;
        case 's':
            if( at(str,i+2) == 'm' ){
                res = true; i += 3;
            }
            break;
        case 'u':
            if( at(str,i+2) == 't' ){
                if( at(str,i+3) == 'o' ){
                    res = true; i += 4;
                }
            }
            break;
        }
        break;
    case 'b':
        switch( at(str,i+1) ){
        case 'i':
            if( at(str,i+2) == 't' ){
                switch( at(str,i+3) ){
                case 'a':
                    if( at(str,i+4) == 'n' ){
                        if( at(str,i+5) == 'd' ){
                            res = true; i += 6;
                        }
                    }
                    break;
                case 'o':
                    if( at(str,i+4) == 'r' ){
                        res = true; i += 5;
                    }
                    break;
                }
            }
            break;
        case 'o':
            if( at(str,i+2) == 'o' ){
                if( at(str,i+3) == 'l' ){
                    res = true; i += 4;
                }
            }
            break;
        case 'r':
            if( at(str,i+2) == 'e' ){
                if( at(str,i+3) == 'a' ){
                    if( at(str,i+4) == 'k' ){
                        res = true; i += 5;
                    }
                }
            }
            break;
        }
        break;
    case 'c':
        switch( at(str,i+1) ){
        case 'a':
            switch( at(str,i+2) ){
            case 's':
                if( at(str,i+3) == 'e' ){
                    res = true; i += 4;
                }
                break;
            case 't':
                if( at(str,i+3) == 'c' ){
                    if( at(str,i+4) == 'h' ){
                        res = true; i += 5;
                    }
                }
                break;
            }
            break;
        case 'h':
            if( at(str,i+2) == 'a' ){
                if( at(str,i+3) == 'r' ){
                    switch( at(str,i+4) ){
                    case '1':
                        if( at(str,i+5) == '6' ){
                            if( at(str,i+6) == '_' ){
                                if( at(str,i+7) == 't' ){
                                    res = true; i += 8;
                                }
                            }
                        }
                        break;
                    case '3':
                        if( at(str,i+5) == '2' ){
                            if( at(str,i+6) == '_' ){
                                if( at(str,i+7) == 't' ){
                                    res = true; i += 8;
                                }
                            }
                        }
                        break;
                    default:
                        res = true; i += 4;
                        break;
                    }
                }
            }
            break;
        case 'l':
            if( at(str,i+2) == 'a' ){
                if( at(str,i+3) == 's' ){
                    if( at(str,i+4) == 's' ){
                        res = true; i += 5;
                    }
                }
            }
            break;
        case 'o':
            switch( at(str,i+2) ){
            case 'm':
                if( at(str,i+3) == 'p' ){
                    if( at(str,i+4) == 'l' ){
                        res = true; i += 5;
                    }
                }
                break;
            case 'n':
                switch( at(str,i+3) ){
                case 's':
                    if( at(str,i+4) == 't' ){
                        switch( at(str,i+5) ){
                        case '_':
                            if( at(str,i+6) == 'c' ){
                                if( at(str,i+7) == 'a' ){
                                    if( at(str,i+8) == 's' ){
                                        if( at(str,i+9) == 't' ){
                                            res = true; i += 10;
                                        }
                                    }
                                }
                            }
                            break;
                        case 'e':
                            if( at(str,i+6) == 'x' ){
                                if( at(str,i+7) == 'p' ){
                                    if( at(str,i+8) == 'r' ){
                                        res = true; i += 9;
                                    }
                                }
                            }
                            break;
                        default:
                            res = true; i += 5;
                            break;
                        }
                    }
                    break;
                case 't':
                    if( at(str,i+4) == 'i' ){
                        if( at(str,i+5) == 'n' ){
                            if( at(str,i+6) == 'u' ){
                                if( at(str,i+7) == 'e' ){
                                    res = true; i += 8;
                                }
                            }
                        }
                    }
                    break;
                }
                break;
            }
            break;
        }
        break;
    case 'd':
        switch( at(str,i+1) ){
        case 'e':
            switch( at(str,i+2) ){
            case 'c':
                if( at(str,i+3) == 'l' ){
                    if( at(str,i+4) == 't' ){
                        if( at(str,i+5) == 'y' ){
                            if( at(str,i+6) == 'p' ){
                                if( at(str,i+7) == 'e' ){
                                    res = true; i += 8;
                                }
                            }
                        }
                    }
                }
                break;
            case 'f':
                if( at(str,i+3) == 'a' ){
                    if( at(str,i+4) == 'u' ){
                        if( at(str,i+5) == 'l' ){
                            if( at(str,i+6) == 't' ){
                                res = true; i += 7;
                            }
                        }
                    }
                }
                break;
            case 'l':
                if( at(str,i+3) == 'e' ){
                    if( at(str,i+4) == 't' ){
                        if( at(str,i+5) == 'e' ){
                            res = true; i += 6;
                        }
                    }
                }
                break;
            }
            break;
        case 'o':
            if( at(str,i+2) == 'u' ){
                if( at(str,i+3) == 'b' ){
                    if( at(str,i+4) == 'l' ){
                        if( at(str,i+5) == 'e' ){
                            res = true; i += 6;
                        }
                    }
                }
            } else {
                res = true; i += 2;
            }
            break;
        case 'y':
            if( at(str,i+2) == 'n' ){
                if( at(str,i+3) == 'a' ){
                    if( at(str,i+4) == 'm' ){
                        if( at(str,i+5) == 'i' ){
                            if( at(str,i+6) == 'c' ){
                                if( at(str,i+7) == '_' ){
                                    if( at(str,i+8) == 'c' ){
                                        if( at(str,i+9) == 'a' ){
                                            if( at(str,i+10) == 's' ){
                                                if( at(str,i+11) == 't' ){
                                                    res = true; i += 12;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            break;
        }
        break;
    case 'e':
        switch( at(str,i+1) ){
        case 'l':
            if( at(str,i+2) == 's' ){
                if( at(str,i+3) == 'e' ){
                    res = true; i += 4;
                }
            }
            break;
        case 'n':
            if( at(str,i+2) == 'u' ){
                if( at(str,i+3) == 'm' ){
                    res = true; i += 4;
                }
            }
            break;
        case 'x':
            switch( at(str,i+2) ){
            case 'p':
                switch( at(str,i+3) ){
                case 'l':
                    if( at(str,i+4) == 'i' ){
                        if( at(str,i+5) == 'c' ){
                            if( at(str,i+6) == 'i' ){
                                if( at(str,i+7) == 't' ){
                                    res = true; i += 8;
                                }
                            }
                        }
                    }
                    break;
                case 'o':
                    if( at(str,i+4) == 'r' ){
                        if( at(str,i+5) == 't' ){
                            res = true; i += 6;
                        }
                    }
                    break;
                }
                break;
            case 't':
                if( at(str,i+3) == 'e' ){
                    if( at(str,i+4) == 'r' ){
                        if( at(str,i+5) == 'n' ){
                            res = true; i += 6;
                        }
                    }
                }
                break;
            }
            break;
        }
        break;
    case 'f':
        switch( at(str,i+1) ){
        case 'a':
            if( at(str,i+2) == 'l' ){
                if( at(str,i+3) == 's' ){
                    if( at(str,i+4) == 'e' ){
                        res = true; i += 5;
                    }
                }
            }
            break;
        case 'l':
            if( at(str,i+2) == 'o' ){
                if( at(str,i+3) == 'a' ){
                    if( at(str,i+4) == 't' ){
                        res = true; i += 5;
                    }
                }
            }
            break;
        case 'o':
            if( at(str,i+2) == 'r' ){
                res = true; i += 3;
            }
            break;
        case 'r':
            if( at(str,i+2) == 'i' ){
                if( at(str,i+3) == 'e' ){
                    if( at(str,i+4) == 'n' ){
                        if( at(str,i+5) == 'd' ){
                            res = true; i += 6;
                        }
                    }
                }
            }
            break;
        }
        break;
    case 'g':
        if( at(str,i+1) == 'o' ){
            if( at(str,i+2) == 't' ){
                if( at(str,i+3) == 'o' ){
                    res = true; i += 4;
                }
            }
        }
        break;
    case 'i':
        switch( at(str,i+1) ){
        case 'f':
            res = true; i += 2;
            break;
        case 'n':
            switch( at(str,i+2) ){
            case 'l':
                if( at(str,i+3) == 'i' ){
                    if( at(str,i+4) == 'n' ){
                        if( at(str,i+5) == 'e' ){
                            res = true; i += 6;
                        }
                    }
                }
                break;
            case 't':
                res = true; i += 3;
                break;
            }
            break;
        }
        break;
    case 'l':
        if( at(str,i+1) == 'o' ){
            if( at(str,i+2) == 'n' ){
                if( at(str,i+3) == 'g' ){
                    res = true; i += 4;
                }
            }
        }
        break;
    case 'm':
        if( at(str,i+1) == 'u' ){
            if( at(str,i+2) == 't' ){
                if( at(str,i+3) == 'a' ){
                    if( at(str,i+4) == 'b' ){
                        if( at(str,i+5) == 'l' ){
                            if( at(str,i+6) == 'e' ){
                                res = true; i += 7;
                            }
                        }
                    }
                }
            }
        }
        break;
    case 'n':
        switch( at(str,i+1) ){
        case 'a':
            if( at(str,i+2) == 'm' ){
                if( at(str,i+3) == 'e' ){
                    if( at(str,i+4) == 's' ){
                        if( at(str,i+5) == 'p' ){
                            if( at(str,i+6) == 'a' ){
                                if( at(str,i+7) == 'c' ){
                                    if( at(str,i+8) == 'e' ){
                                        res = true; i += 9;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            break;
        case 'e':
            if( at(str,i+2) == 'w' ){
                res = true; i += 3;
            }
            break;
        case 'o':
            switch( at(str,i+2) ){
            case 'e':
                if( at(str,i+3) == 'x' ){
                    if( at(str,i+4) == 'c' ){
                        if( at(str,i+5) == 'e' ){
                            if( at(str,i+6) == 'p' ){
                                if( at(str,i+7) == 't' ){
                                    res = true; i += 8;
                                }
                            }
                        }
                    }
                }
                break;
            case 't':
                if( at(str,i+3) == '_' ){
                    if( at(str,i+4) == 'e' ){
                        if( at(str,i+5) == 'q' ){
                            res = true; i += 6;
                        }
                    }
                } else {
                    res = true; i += 3;
                }
                break;
            }
            break;
        case 'u':
            if( at(str,i+2) == 'l' ){
                if( at(str,i+3) == 'l' ){
                    if( at(str,i+4) == 'p' ){
                        if( at(str,i+5) == 't' ){
                            if( at(str,i+6) == 'r' ){
                                res = true; i += 7;
                            }
                        }
                    }
                }
            }
            break;
        }
        break;
    case 'o':
        switch( at(str,i+1) ){
        case 'p':
            if( at(str,i+2) == 'e' ){
                if( at(str,i+3) == 'r' ){
                    if( at(str,i+4) == 'a' ){
                        if( at(str,i+5) == 't' ){
                            if( at(str,i+6) == 'o' ){
                                if( at(str,i+7) == 'r' ){
                                    res = true; i += 8;
                                }
                            }
                        }
                    }
                }
            }
            break;
        case 'r':
            if( at(str,i+2) == '_' ){
                if( at(str,i+3) == 'e' ){
                    if( at(str,i+4) == 'q' ){
                        res = true; i += 5;
                    }
                }
            } else {
                res = true; i += 2;
            }
            break;
        }
        break;
    case 'p':
        switch( at(str,i+1) ){
        case 'r':
            switch( at(str,i+2) ){
            case 'i':
                if( at(str,i+3) == 'v' ){
                    if( at(str,i+4) == 'a' ){
                        if( at(str,i+5) == 't' ){
                            if( at(str,i+6) == 'e' ){
                                res = true; i += 7;
                            }
                        }
                    }
                }
                break;
            case 'o':
                if( at(str,i+3) == 't' ){
                    if( at(str,i+4) == 'e' ){
                        if( at(str,i+5) == 'c' ){
                            if( at(str,i+6) == 't' ){
                                if( at(str,i+7) == 'e' ){
                                    if( at(str,i+8) == 'd' ){
                                        res = true; i += 9;
                                    }
                                }
                            }
                        }
                    }
                }
                break;
            }
            break;
        case 'u':
            if( at(str,i+2) == 'b' ){
                if( at(str,i+3) == 'l' ){
                    if( at(str,i+4) == 'i' ){
                        if( at(str,i+5) == 'c' ){
                            res = true; i += 6;
                        }
                    }
                }
            }
            break;
        }
        break;
    case 'r':
        if( at(str,i+1) == 'e' ){
            switch( at(str,i+2) ){
            case 'g':
                if( at(str,i+3) == 'i' ){
                    if( at(str,i+4) == 's' ){
                        if( at(str,i+5) == 't' ){
                            if( at(str,i+6) == 'e' ){
                                if( at(str,i+7) == 'r' ){
                                    res = true; i += 8;
                                }
                            }
                        }
                    }
                }
                break;
            case 'i':
                if( at(str,i+3) == 'n' ){
                    if( at(str,i+4) == 't' ){
                        if( at(str,i+5) == 'e' ){
                            if( at(str,i+6) == 'r' ){
                                if( at(str,i+7) == 'p' ){
                                    if( at(str,i+8) == 'r' ){
                                        if( at(str,i+9) == 'e' ){
                                            if( at(str,i+10) == 't' ){
                                                if( at(str,i+11) == '_' ){
                                                    if( at(str,i+12) == 'c' ){
                                                        if( at(str,i+13) == 'a' ){
                                                            if( at(str,i+14) == 's' ){
                                                                if( at(str,i+15) == 't' ){
                                                                    res = true; i += 16;
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                break;
            case 't':
                if( at(str,i+3) == 'u' ){
                    if( at(str,i+4) == 'r' ){
                        if( at(str,i+5) == 'n' ){
                            res = true; i += 6;
                        }
                    }
                }
                break;
            }
        }
        break;
    case 's':
        switch( at(str,i+1) ){
        case 'h':
            if( at(str,i+2) == 'o' ){
                if( at(str,i+3) == 'r' ){
                    if( at(str,i+4) == 't' ){
                        res = true; i += 5;
                    }
                }
            }
            break;
        case 'i':
            switch( at(str,i+2) ){
            case 'g':
                if( at(str,i+3) == 'n' ){
                    if( at(str,i+4) == 'e' ){
                        if( at(str,i+5) == 'd' ){
                            res = true; i += 6;
                        }
                    }
                }
                break;
            case 'z':
                if( at(str,i+3) == 'e' ){
                    if( at(str,i+4) == 'o' ){
                        if( at(str,i+5) == 'f' ){
                            res = true; i += 6;
                        }
                    }
                }
                break;
            }
            break;
        case 't':
            switch( at(str,i+2) ){
            case 'a':
                if( at(str,i+3) == 't' ){
                    if( at(str,i+4) == 'i' ){
                        if( at(str,i+5) == 'c' ){
                            if( at(str,i+6) == '_' ){
                                switch( at(str,i+7) ){
                                case 'a':
                                    if( at(str,i+8) == 's' ){
                                        if( at(str,i+9) == 's' ){
                                            if( at(str,i+10) == 'e' ){
                                                if( at(str,i+11) == 'r' ){
                                                    if( at(str,i+12) == 't' ){
                                                        res = true; i += 13;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    break;
                                case 'c':
                                    if( at(str,i+8) == 'a' ){
                                        if( at(str,i+9) == 's' ){
                                            if( at(str,i+10) == 't' ){
                                                res = true; i += 11;
                                            }
                                        }
                                    }
                                    break;
                                }
                            } else {
                                res = true; i += 6;
                            }
                        }
                    }
                }
                break;
            case 'r':
                if( at(str,i+3) == 'u' ){
                    if( at(str,i+4) == 'c' ){
                        if( at(str,i+5) == 't' ){
                            res = true; i += 6;
                        }
                    }
                }
                break;
            }
            break;
        case 'w':
            if( at(str,i+2) == 'i' ){
                if( at(str,i+3) == 't' ){
                    if( at(str,i+4) == 'c' ){
                        if( at(str,i+5) == 'h' ){
                            res = true; i += 6;
                        }
                    }
                }
            }
            break;
        }
        break;
    case 't':
        switch( at(str,i+1) ){
        case 'e':
            if( at(str,i+2) == 'm' ){
                if( at(str,i+3) == 'p' ){
                    if( at(str,i+4) == 'l' ){
                        if( at(str,i+5) == 'a' ){
                            if( at(str,i+6) == 't' ){
                                if( at(str,i+7) == 'e' ){
                                    res = true; i += 8;
                                }
                            }
                        }
                    }
                }
            }
            break;
        case 'h':
            switch( at(str,i+2) ){
            case 'i':
                if( at(str,i+3) == 's' ){
                    res = true; i += 4;
                }
                break;
            case 'r':
                switch( at(str,i+3) ){
                case 'e':
                    if( at(str,i+4) == 'a' ){
                        if( at(str,i+5) == 'd' ){
                            if( at(str,i+6) == '_' ){
                                if( at(str,i+7) == 'l' ){
                                    if( at(str,i+8) == 'o' ){
                                        if( at(str,i+9) == 'c' ){
                                            if( at(str,i+10) == 'a' ){
                                                if( at(str,i+11) == 'l' ){
                                                    res = true; i += 12;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    break;
                case 'o':
                    if( at(str,i+4) == 'w' ){
                        res = true; i += 5;
                    }
                    break;
                }
                break;
            }
            break;
        case 'r':
            switch( at(str,i+2) ){
            case 'u':
                if( at(str,i+3) == 'e' ){
                    res = true; i += 4;
                }
                break;
            case 'y':
                res = true; i += 3;
                break;
            }
            break;
        case 'y':
            if( at(str,i+2) == 'p' ){
                if( at(str,i+3) == 'e' ){
                    switch( at(str,i+4) ){
                    case 'd':
                        if( at(str,i+5) == 'e' ){
                            if( at(str,i+6) == 'f' ){
                                res = true; i += 7;
                            }
                        }
                        break;
                    case 'i':
                        if( at(str,i+5) == 'd' ){
                            res = true; i += 6;
                        }
                        break;
                    case 'n':
                        if( at(str,i+5) == 'a' ){
                            if( at(str,i+6) == 'm' ){
                                if( at(str,i+7) == 'e' ){
                                    res = true; i += 8;
                                }
                            }
                        }
                        break;
                    }
                }
            }
            break;
        }
        break;
    case 'u':
        switch( at(str,i+1) ){
        case 'n':
            switch( at(str,i+2) ){
            case 'i':
                if( at(str,i+3) == 'o' ){
                    if( at(str,i+4) == 'n' ){
                        res = true; i += 5;
                    }
                }
                break;
            case 's':
                if( at(str,i+3) == 'i' ){
                    if( at(str,i+4) == 'g' ){
                        if( at(str,i+5) == 'n' ){
                            if( at(str,i+6) == 'e' ){
                                if( at(str,i+7) == 'd' ){
                                    res = true; i += 8;
                                }
                            }
                        }
                    }
                }
                break;
            }
            break;
        case 's':
            if( at(str,i+2) == 'i' ){
                if( at(str,i+3) == 'n' ){
                    if( at(str,i+4) == 'g' ){
                        res = true; i += 5;
                    }
                }
            }
            break;
        }
        break;
    case 'v':
        switch( at(str,i+1) ){
        case 'i':
            if( at(str,i+2) == 'r' ){
                if( at(str,i+3) == 't' ){
                    if( at(str,i+4) == 'u' ){
                        if( at(str,i+5) == 'a' ){
                            if( at(str,i+6) == 'l' ){
                                res = true; i += 7;
                            }
                        }
                    }
                }
            }
            break;
        case 'o':
            switch( at(str,i+2) ){
            case 'i':
                if( at(str,i+3) == 'd' ){
                    res = true; i += 4;
                }
                break;
            case 'l':
                if( at(str,i+3) == 'a' ){
                    if( at(str,i+4) == 't' ){
                        if( at(str,i+5) == 'i' ){
                            if( at(str,i+6) == 'l' ){
                                if( at(str,i+7) == 'e' ){
                                    res = true; i += 8;
                                }
                            }
                        }
                    }
                }
                break;
            }
            break;
        }
        break;
    case 'w':
        switch( at(str,i+1) ){
        case 'c':
            if( at(str,i+2) == 'h' ){
                if( at(str,i+3) == 'a' ){
                    if( at(str,i+4) == 'r' ){
                        if( at(str,i+5) == '_' ){
                            if( at(str,i+6) == 't' ){
                                res = true; i += 7;
                            }
                        }
                    }
                }
            }
            break;
        case 'h':
            if( at(str,i+2) == 'i' ){
                if( at(str,i+3) == 'l' ){
                    if( at(str,i+4) == 'e' ){
                        res = true; i += 5;
                    }
                }
            }
            break;
        }
        break;
    case 'x':
        if( at(str,i+1) == 'o' ){
            if( at(str,i+2) == 'r' ){
                if( at(str,i+3) == '_' ){
                    if( at(str,i+4) == 'e' ){
                        if( at(str,i+5) == 'q' ){
                            res = true; i += 6;
                        }
                    }
                } else {
                    res = true; i += 3;
                }
            }
        }
        break;
    }
    return res;
}

