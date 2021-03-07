#ifndef OBXLIBFFI_H
#define OBXLIBFFI_H

#include <QString>

typedef struct lua_State lua_State;

namespace Obx
{
    typedef void (*SendToLog)( const QString& );

    struct LibFfi
    {
        static void setSendToLog(SendToLog);
        static void install(lua_State *L);
    };
}

#endif // OBXLIBFFI_H

