#include "ObRowCol.h"
#include <QtDebug>
using namespace Ob;

RowCol::RowCol(quint32 row, quint32 col)
{
    if( !setRowCol(row,col) )
        qWarning() << "exceeding maximum row or column number";
}

bool RowCol::setRowCol(quint32 row, quint32 col)
{
    static const quint32 maxRow = ( 1 << ROW_BIT_LEN ) - 1;
    static const quint32 maxCol = ( 1 << COL_BIT_LEN ) - 1;
    int err = 0;
    if( row > maxRow )
    {
        d_row = maxRow;
        err++;
    }else
        d_row = row;
    if( col > maxCol )
    {
        d_col = maxCol;
        err++;
    }else
        d_col = col;
    return err == 0;
}
