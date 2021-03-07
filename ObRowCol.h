#ifndef OBROWCOL_H
#define OBROWCOL_H

#include <QString>

namespace Ob
{
    struct RowCol
    {
        enum { ROW_BIT_LEN = 19, COL_BIT_LEN = 32 - ROW_BIT_LEN - 1, MSB = 0x80000000 };
        uint d_row : ROW_BIT_LEN; // supports 524k lines
        uint d_col : COL_BIT_LEN; // supports 4k chars per line
        uint unused : 1;
        RowCol():d_row(0),d_col(0) {}
        RowCol( quint32 row, quint32 col );
        bool setRowCol( quint32 row, quint32 col );
        bool isValid() const { return d_row > 0 && d_col > 0; } // valid lines and cols start with 1; 0 is invalid
        quint32 packed() const { return ( d_row << COL_BIT_LEN ) | d_col | MSB; }
        static bool isPacked( quint32 rowCol ) { return rowCol & MSB; }
        static quint32 unpackCol(quint32 rowCol ) { return rowCol & ( ( 1 << COL_BIT_LEN ) -1 ); }
        static quint32 unpackCol2(quint32 rowCol ) { return isPacked(rowCol) ? unpackCol(rowCol) : 1; }
        static quint32 unpackRow(quint32 rowCol ) { return ( ( rowCol & ~MSB ) >> COL_BIT_LEN ); }
        static quint32 unpackRow2(quint32 rowCol ) { return isPacked(rowCol) ? unpackRow(rowCol) : rowCol; }
        quint32 line() const { return d_row; }
        bool operator==( const RowCol& rhs ) const { return d_row == rhs.d_row && d_col == rhs.d_col; }
    };

    struct Loc : public RowCol
    {
        Loc( quint32 row, quint32 col, const QString& f ):RowCol( row, col ),d_file(f) {}
        Loc( const RowCol& r, const QString& f):RowCol(r),d_file(f) {}
        QString d_file;
    };
}

#endif // OBROWCOL_H
