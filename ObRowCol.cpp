#include "ObRowCol.h"
#include <QtDebug>
using namespace Ob;

RowCol::RowCol(quint32 row, quint32 col)
{
    if( !setRowCol(row,col) )
        qWarning() << "invalid row or column number";
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
    }else if( row == 0 )
    {
        d_row = 1;
        err++;
    }else
        d_row = row;
    if( col > maxCol )
    {
        d_col = maxCol;
        err++;
    }else if( col == 0 )
    {
        d_col = 1;
        err++;
    }else
        d_col = col;
    return err == 0;
}
