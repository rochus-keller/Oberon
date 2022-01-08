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

#include "ObRowCol.h"
#include "ObxIlEmitter.h"
#include <QCoreApplication>
#include <QDateTime>
using namespace Obx;

IlEmitter::IlEmitter(IlRenderer* r):d_out(r)
{
    Q_ASSERT( r );
}

void IlEmitter::beginModule(const QByteArray& assemblyName, const QByteArray& moduleName, const QByteArrayList& imports, const QString& sourceFile, IlEmitter::ModuleKind k)
{
    Q_ASSERT( !moduleName.isEmpty() );
    d_out->beginModule(assemblyName,moduleName,imports,sourceFile,k);
}

void IlEmitter::endModule()
{
    d_out->endModule();
}

void IlEmitter::beginMethod(const QByteArray& methodName, bool isPublic, IlEmitter::MethodKind k, bool isRuntime)
{
    Q_ASSERT( d_method.isEmpty() );
    Q_ASSERT(!methodName.isEmpty());
    d_method = methodName;
    d_isPublic = isPublic;
    d_isRuntime = isRuntime;
    d_methodKind = k;
    d_labelCount = 1;
    d_body.clear();
    d_args.clear();
    d_locals.clear();
    d_stackDepth = 0;
    d_maxStackDepth = 0;
    d_retType.clear();
    d_library.clear();
    d_origName.clear();
    d_isVararg = false;
}

void IlEmitter::endMethod()
{
    IlMethod meth;
    meth.d_args = d_args;
    meth.d_body = d_body;
    meth.d_isPublic = d_isPublic;
    meth.d_isRuntime = d_isRuntime;
    meth.d_isVararg = d_isVararg;
    meth.d_locals = d_locals;
    meth.d_methodKind = d_methodKind;
    meth.d_name = d_method;
    meth.d_retType = d_retType;
    meth.d_library = d_library;
    meth.d_origName = d_origName;
    meth.d_stackDepth = d_maxStackDepth;
    d_out->addMethod(meth);
    d_method.clear();
    d_body.clear();
    d_retType.clear();
    d_args.clear();
    d_locals.clear();
    d_library.clear();
    d_origName.clear();
}

void IlEmitter::beginClass(const QByteArray& className, bool isPublic, quint8 classKind, const QByteArray& superClassRef, int byteSize)
{
    d_out->beginClass(className,isPublic, classKind, superClassRef, byteSize);
}

void IlEmitter::endClass()
{
    d_out->endClass();
}

void IlEmitter::addField(const QByteArray& fieldName, const QByteArray& typeRef, bool isPublic, bool isStatic, int explicitOffset, const QByteArray& marshalAs)
{
    d_out->addField(fieldName, typeRef, isPublic, isStatic, explicitOffset, marshalAs );
}

quint32 IlEmitter::addLocal(const QByteArray& typeRef, QByteArray name)
{
    Q_ASSERT( !d_method.isEmpty() );
    Q_ASSERT( !typeRef.isEmpty() );
    d_locals.append(qMakePair(typeRef,name));
    return 0;
}

quint32 IlEmitter::addArgument(const QByteArray& typeRef, QByteArray name)
{
    Q_ASSERT( !d_method.isEmpty() );
    Q_ASSERT( !typeRef.isEmpty() );
    d_args.append(qMakePair(typeRef,name));
    return 0;
}

void IlEmitter::setReturnType(const QByteArray& typeRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    Q_ASSERT( d_retType.isEmpty() );
    d_retType = typeRef;
}

void IlEmitter::setPinvoke(const QByteArray& lib, const QByteArray& origName)
{
    Q_ASSERT( !d_method.isEmpty() );
    Q_ASSERT( d_library.isEmpty() );
    d_library = lib;
    d_origName = origName;
}

void IlEmitter::setVararg()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_isVararg = true;
}

quint32 IlEmitter::newLabel()
{
    return d_labelCount++;
}

void IlEmitter::label_(quint32 label)
{
    Q_ASSERT( !d_method.isEmpty() );
    Q_ASSERT( label <= 0xffffff );
    d_body.append(IlOperation(IL_label,label) );
    delta(0);
}

void IlEmitter::line_(const Ob::RowCol& loc)
{
    d_body.append(IlOperation(IL_line,QByteArray::number(loc.d_row)+":"+QByteArray::number(loc.d_col)) );
    delta(0);
}

void IlEmitter::add_(bool withOverflow, bool withUnsignedOverflow)
{
    Q_ASSERT( !d_method.isEmpty() );
    if( withUnsignedOverflow )
        d_body.append(IlOperation(IL_add_ovf_un) );
    else if( withOverflow )
        d_body.append(IlOperation(IL_add_ovf) );
    else
        d_body.append(IlOperation(IL_add) );
    delta(-2+1);
}

void IlEmitter::and_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_and) );
    delta(-2+1);
}

void IlEmitter::beq_(quint32 label)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_beq,label) );
    delta(-2);
}

void IlEmitter::bge_(quint32 label, bool withUnsigned)
{
    Q_ASSERT( !d_method.isEmpty() );
    if( withUnsigned )
        d_body.append(IlOperation(IL_bge_un,label) );
    else
        d_body.append(IlOperation(IL_bge,label) );
    delta(-2);
}

void IlEmitter::bgt_(quint32 label, bool withUnsigned)
{
    Q_ASSERT( !d_method.isEmpty() );
    if( withUnsigned )
        d_body.append(IlOperation(IL_bgt_un,label) );
    else
        d_body.append(IlOperation(IL_bgt,label) );
    delta(-2);
}

void IlEmitter::ble_(quint32 label, bool withUnsigned)
{
    Q_ASSERT( !d_method.isEmpty() );
    if( withUnsigned )
        d_body.append(IlOperation(IL_ble_un,label) );
    else
        d_body.append(IlOperation(IL_ble,label) );
    delta(-2);
}

void IlEmitter::blt_(quint32 label, bool withUnsigned)
{
    Q_ASSERT( !d_method.isEmpty() );
    if( withUnsigned )
        d_body.append(IlOperation(IL_blt_un,label) );
    else
        d_body.append(IlOperation(IL_blt,label) );
    delta(-2);
}

void IlEmitter::bne_(quint32 label)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_bne_un,label) );
    delta(-2);
}

void IlEmitter::box_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_box,typeRef) );
    delta(-1+1);
}

void IlEmitter::br_(quint32 label)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_br,label) );
    delta(0);
}

void IlEmitter::break_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_break) );
    delta(0);
}

void IlEmitter::brfalse_(quint32 label)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_brfalse,label) );
    delta(-1);
}

void IlEmitter::brnull_(quint32 label)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_brnull,label) );
    delta(-1);
}

void IlEmitter::brzero_(quint32 label)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_brzero,label) );
    delta(-1);
}

void IlEmitter::brtrue_(quint32 label)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_brtrue,label) );
    delta(-1);
}

void IlEmitter::brinst_(quint32 label)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_brinst,label) );
    delta(-1);
}

void IlEmitter::call_(const QByteArray& methodRef, int argCount, bool hasRet, bool isInstance)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_call,methodRef,isInstance) );
    delta(-argCount + (hasRet?1:0) );
}

void IlEmitter::callvirt_(const QByteArray& methodRef, int argCount, bool hasRet)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_callvirt,methodRef) );
    delta(-argCount + (hasRet?1:0) );
}

void IlEmitter::castclass_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_castclass,typeRef) );
    delta(-1+1);
}

void IlEmitter::ceq_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_ceq));
    delta(-2+1);
}

void IlEmitter::cgt_(bool withUnsigned)
{
    Q_ASSERT( !d_method.isEmpty() );
    if( withUnsigned )
        d_body.append(IlOperation(IL_cgt_un) );
    else
        d_body.append(IlOperation(IL_cgt) );
    delta(-2+1);
}

void IlEmitter::clt_(bool withUnsigned)
{
    Q_ASSERT( !d_method.isEmpty() );
    if( withUnsigned )
        d_body.append(IlOperation(IL_clt_un) );
    else
        d_body.append(IlOperation(IL_clt) );
    delta(-2+1);
}

void IlEmitter::cpblk_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_cpblk));
    delta(-3);
}

void IlEmitter::conv_(IlEmitter::ToType t, bool withOverflow, bool withUnsignedOverflow)
{
    Q_ASSERT( !d_method.isEmpty() );
    switch( t )
    {
    case ToI1:
        if( withUnsignedOverflow )
            d_body.append(IlOperation(IL_conv_ovf_i1_un));
        else if( withOverflow )
            d_body.append(IlOperation(IL_conv_ovf_i1));
        else
            d_body.append(IlOperation(IL_conv_i1));
        break;
    case ToI2:
        if( withUnsignedOverflow )
            d_body.append(IlOperation(IL_conv_ovf_i2_un));
        else if( withOverflow )
            d_body.append(IlOperation(IL_conv_ovf_i2));
        else
            d_body.append(IlOperation(IL_conv_i2));
        break;
    case ToI4:
        if( withUnsignedOverflow )
            d_body.append(IlOperation(IL_conv_ovf_i4_un));
        else if( withOverflow )
            d_body.append(IlOperation(IL_conv_ovf_i4));
        else
            d_body.append(IlOperation(IL_conv_i4));
        break;
    case ToI8:
        if( withUnsignedOverflow )
            d_body.append(IlOperation(IL_conv_ovf_i8_un));
        else if( withOverflow )
            d_body.append(IlOperation(IL_conv_ovf_i8));
        else
            d_body.append(IlOperation(IL_conv_i8));
        break;
    case ToR4:
        d_body.append(IlOperation(IL_conv_r4));
        break;
    case ToR8:
        d_body.append(IlOperation(IL_conv_r8));
        break;
    case ToU1:
        if( withUnsignedOverflow )
            d_body.append(IlOperation(IL_conv_ovf_u1_un));
        else if( withOverflow )
            d_body.append(IlOperation(IL_conv_ovf_u1));
        else
            d_body.append(IlOperation(IL_conv_u1));
        break;
    case ToU2:
        if( withUnsignedOverflow )
            d_body.append(IlOperation(IL_conv_ovf_u2_un));
        else if( withOverflow )
            d_body.append(IlOperation(IL_conv_ovf_u2));
        else
            d_body.append(IlOperation(IL_conv_u2));
        break;
    case ToU4:
        if( withUnsignedOverflow )
            d_body.append(IlOperation(IL_conv_ovf_u4_un));
        else if( withOverflow )
            d_body.append(IlOperation(IL_conv_ovf_u4));
        else
            d_body.append(IlOperation(IL_conv_u4));
        break;
    case ToU8:
        if( withUnsignedOverflow )
            d_body.append(IlOperation(IL_conv_ovf_u8_un));
        else if( withOverflow )
            d_body.append(IlOperation(IL_conv_ovf_u8));
        else
            d_body.append(IlOperation(IL_conv_u8));
        break;
    default:
        Q_ASSERT(false);
        break;
    }
    delta(-1+1);
}

void IlEmitter::div_(bool withUnsigned)
{
    Q_ASSERT( !d_method.isEmpty() );
    if( withUnsigned )
        d_body.append(IlOperation(IL_div_un ) );
    else
        d_body.append(IlOperation(IL_div ) );
    delta(-2+1);
}

void IlEmitter::dup_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_dup ) );
    delta(+1);
}

void IlEmitter::initblk_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_initblk ) );
    delta(-3);
}

void IlEmitter::initobj_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_initobj,typeRef) );
    delta(-1);
}

void IlEmitter::isinst_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_isinst,typeRef) );
    delta(-1+1);
}

void IlEmitter::ldarg_(quint16 arg)
{
    Q_ASSERT( !d_method.isEmpty() );
    switch(arg)
    {
    case 0:
        d_body.append(IlOperation(IL_ldarg_0) );
        break;
    case 1:
        d_body.append(IlOperation(IL_ldarg_1) );
        break;
    case 2:
        d_body.append(IlOperation(IL_ldarg_2) );
        break;
    case 3:
        d_body.append(IlOperation(IL_ldarg_3) );
        break;
    default:
        if( arg >= 4 && arg <= 255 )
            d_body.append(IlOperation(IL_ldarg_s,arg) );
        else
            d_body.append(IlOperation(IL_ldarg,arg) );
   }
    delta(+1);
}

void IlEmitter::ldarga_(quint16 arg)
{
    Q_ASSERT( !d_method.isEmpty() );
    if( arg <= 255 )
        d_body.append(IlOperation(IL_ldarga_s,arg) );
    else
        d_body.append(IlOperation(IL_ldarga,arg) );
    delta(+1);
}

void IlEmitter::ldc_i4(qint32 v)
{
    Q_ASSERT( !d_method.isEmpty() );
    switch(v)
    {
    case 0:
        d_body.append(IlOperation(IL_ldc_i4_0));
        break;
    case 1:
        d_body.append(IlOperation(IL_ldc_i4_1));
        break;
    case 2:
        d_body.append(IlOperation(IL_ldc_i4_2));
        break;
    case 3:
        d_body.append(IlOperation(IL_ldc_i4_3));
        break;
    case 4:
        d_body.append(IlOperation(IL_ldc_i4_4));
        break;
    case 5:
        d_body.append(IlOperation(IL_ldc_i4_5));
        break;
    case 6:
        d_body.append(IlOperation(IL_ldc_i4_6));
        break;
    case 7:
        d_body.append(IlOperation(IL_ldc_i4_7));
        break;
    case 8:
        d_body.append(IlOperation(IL_ldc_i4_8));
        break;
    case -1:
        d_body.append(IlOperation(IL_ldc_i4_m1));
        break;
    default:
        if( v >= -128 && v <= 127 )
            d_body.append(IlOperation(IL_ldc_i4_s,v) );
        else
            d_body.append(IlOperation(IL_ldc_i4,QByteArray::number(v)) );
    }
    delta(+1);
}

void IlEmitter::ldc_i8(qint64 v)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_ldc_i8,QByteArray::number(v)) );
    delta(+1);
}

void IlEmitter::ldc_r4(double v)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_ldc_r4,QByteArray::number(v,'g',9)) );
    delta(+1);
}

void IlEmitter::ldc_r8(double v)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_ldc_r8,QByteArray::number(v,'g',17)) );
    delta(+1);
}

void IlEmitter::ldelem_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    if( typeRef == "int8" )
        d_body.append(IlOperation(IL_ldelem_i1));
    else if( typeRef == "int16" )
        d_body.append(IlOperation(IL_ldelem_i2));
    else if( typeRef == "int32" )
        d_body.append(IlOperation(IL_ldelem_i4));
    else if( typeRef == "int64" )
        d_body.append(IlOperation(IL_ldelem_i8));
    else if( typeRef == "float32" )
        d_body.append(IlOperation(IL_ldelem_r4));
    else if( typeRef == "float64" )
        d_body.append(IlOperation(IL_ldelem_r8));
    else if( typeRef == "uint8" )
        d_body.append(IlOperation(IL_ldelem_u1));
    else if( typeRef == "uint16" )
        d_body.append(IlOperation(IL_ldelem_u2));
    else if( typeRef == "uint32" )
        d_body.append(IlOperation(IL_ldelem_u4));
    else if( typeRef == "uint64" )
        d_body.append(IlOperation(IL_ldelem_u8));
    else
        d_body.append(IlOperation(IL_ldelem,typeRef));
    delta(-2+1);
}

void IlEmitter::ldelema_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_ldelema,typeRef));
    delta(-2+1);
}

void IlEmitter::ldfld_(const QByteArray& fieldRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_ldfld,fieldRef));
    delta(-1+1);
}

void IlEmitter::ldflda_(const QByteArray& fieldRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_ldflda,fieldRef));
    delta(-1+1);
}

void IlEmitter::ldftn_(const QByteArray& methodRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_ldftn,methodRef));
    delta(+1);
}

void IlEmitter::ldind_(IlEmitter::IndType t)
{
    Q_ASSERT( !d_method.isEmpty() );
    IL_op i = IL_invalid;
    switch( t )
    {
    case I1:
        i = IL_ldind_i1;
        break;
    case I2:
        i = IL_ldind_i2;
        break;
    case I4:
        i = IL_ldind_i4;
        break;
    case I8:
        i = IL_ldind_i8;
        break;
    case U1:
        i = IL_ldind_u1;
        break;
    case U2:
        i = IL_ldind_u2;
        break;
    case U4:
        i = IL_ldind_u4;
        break;
    case U8:
        i = IL_ldind_u8;
        break;
    case R4:
        i = IL_ldind_r4;
        break;
    case R8:
        i = IL_ldind_r8;
        break;
    case Ref:
        i = IL_ldind_ref;
        break;
    case IntPtr:
        i = IL_ldind_i;
        break;
    default:
        Q_ASSERT( false );
    }
    d_body.append(IlOperation(i));
    delta(-1+1);
}

void IlEmitter::ldlen_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_ldlen));
    delta(-1+1);
}

void IlEmitter::ldloc_(quint16 loc)
{
    Q_ASSERT( !d_method.isEmpty() );
    switch(loc)
    {
    case 0:
        d_body.append(IlOperation(IL_ldloc_0) );
        break;
    case 1:
        d_body.append(IlOperation(IL_ldloc_1) );
        break;
    case 2:
        d_body.append(IlOperation(IL_ldloc_2) );
        break;
    case 3:
        d_body.append(IlOperation(IL_ldloc_3) );
        break;
    default:
        if( loc >= 4 && loc <= 255 )
            d_body.append(IlOperation(IL_ldloc_s,loc) );
        else
            d_body.append(IlOperation(IL_ldloc,loc) );
   }
    delta(+1);
}

void IlEmitter::ldloca_(quint16 loc)
{
    Q_ASSERT( !d_method.isEmpty() );
    if( loc <= 255 )
        d_body.append(IlOperation(IL_ldloca_s,loc));
    else
        d_body.append(IlOperation(IL_ldloca,loc));
    delta(+1);
}

void IlEmitter::ldnull_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_ldnull));
    delta(+1);
}

void IlEmitter::ldobj_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_ldobj,typeRef));
    delta(-1+1);
}

void IlEmitter::ldsfld_(const QByteArray& fieldRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_ldsfld, fieldRef));
    delta(+1);
}

void IlEmitter::ldsflda_(const QByteArray& fieldRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_ldsflda,fieldRef));
    delta(+1);
}

void IlEmitter::ldstr_(const QByteArray& utf8)
{
    Q_ASSERT( !d_method.isEmpty() );
    Q_ASSERT( !utf8.isEmpty() && utf8.startsWith('"') && utf8.endsWith('"') );
    // expecting a string in "" and properly escaped
    d_body.append(IlOperation(IL_ldstr,utf8));
    delta(+1);
}

void IlEmitter::ldvirtftn_(const QByteArray& methodRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_ldvirtftn,methodRef));
    delta(-1+1);
}

void IlEmitter::localloc_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_localloc));
    delta(-1+1);
}

void IlEmitter::mul_(bool withOverflow, bool withUnsignedOverflow)
{
    Q_ASSERT( !d_method.isEmpty() );
    if( withUnsignedOverflow )
        d_body.append(IlOperation(IL_mul_ovf_un ) );
    else if( withOverflow )
        d_body.append(IlOperation(IL_mul_ovf ) );
    else
        d_body.append(IlOperation(IL_mul ) );
    delta(-2+1);
}

void IlEmitter::neg_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_neg ) );
    delta(-1+1);
}

void IlEmitter::newarr_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_newarr,typeRef));
    delta(-1+1);
}

void IlEmitter::newobj_(const QByteArray& methodRef, int argCount)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_newobj,methodRef));
    delta(-argCount+1);
}

void IlEmitter::nop_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_nop));
    delta(0);
}

void IlEmitter::not_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_not));
    delta(-1+1);
}

void IlEmitter::or_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_or));
    delta(-2+1);
}

void IlEmitter::pop_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_pop));
    delta(-1);
}

void IlEmitter::rem_(bool withUnsigned)
{
    Q_ASSERT( !d_method.isEmpty() );
    if( withUnsigned )
        d_body.append(IlOperation(IL_rem_un ) );
    else
        d_body.append(IlOperation(IL_rem ) );
    delta(-2+1);
}

void IlEmitter::ret_(bool hasRet)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_ret));
    delta( hasRet ? -1: 0 );
}

void IlEmitter::shl_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_shl));
    delta(-2+1);
}

void IlEmitter::shr_(bool withUnsigned)
{
    Q_ASSERT( !d_method.isEmpty() );
    if( withUnsigned )
        d_body.append(IlOperation(IL_shr_un ) );
    else
        d_body.append(IlOperation(IL_shr ) );
    delta(-2+1);
}

void IlEmitter::starg_(quint16 arg)
{
    Q_ASSERT( !d_method.isEmpty() );
    if( arg <= 255 )
        d_body.append(IlOperation(IL_starg_s,arg ) );
    else
        d_body.append(IlOperation(IL_starg,arg ) );
    delta(-1);
}

void IlEmitter::stelem_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    if( typeRef == "int8" )
        d_body.append(IlOperation(IL_stelem_i1));
    else if( typeRef == "int16" )
        d_body.append(IlOperation(IL_stelem_i2));
    else if( typeRef == "int32" )
        d_body.append(IlOperation(IL_stelem_i4));
    else if( typeRef == "int64" )
        d_body.append(IlOperation(IL_stelem_i8));
    else if( typeRef == "float32" )
        d_body.append(IlOperation(IL_stelem_r4));
    else if( typeRef == "float64" )
        d_body.append(IlOperation(IL_stelem_r8));
    else
        d_body.append(IlOperation(IL_stelem,typeRef));
    delta(-3);
}

void IlEmitter::stfld_(const QByteArray& fieldRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_stfld,fieldRef ) );
    delta(-2);
}

void IlEmitter::stind_(IlEmitter::IndType t)
{
    Q_ASSERT( !d_method.isEmpty() );
    IL_op i = IL_invalid;
    switch( t )
    {
    case I1:
        i = IL_stind_i1;
        break;
    case I2:
        i = IL_stind_i2;
        break;
    case I4:
        i = IL_stind_i4;
        break;
    case I8:
        i = IL_stind_i8;
        break;
    case R4:
        i = IL_stind_r4;
        break;
    case R8:
        i = IL_stind_r8;
        break;
    case Ref:
        i = IL_stind_ref;
        break;
    case IntPtr:
        i = IL_stind_i;
        break;
    default:
        Q_ASSERT( false );
    }
    d_body.append(IlOperation(i));
    delta(-2);
}

void IlEmitter::stloc_(quint16 loc)
{
    Q_ASSERT( !d_method.isEmpty() );
    switch(loc)
    {
    case 0:
        d_body.append(IlOperation(IL_stloc_0) );
        break;
    case 1:
        d_body.append(IlOperation(IL_stloc_1) );
        break;
    case 2:
        d_body.append(IlOperation(IL_stloc_2) );
        break;
    case 3:
        d_body.append(IlOperation(IL_stloc_3) );
        break;
    default:
        if( loc >= 4 && loc <= 255 )
            d_body.append(IlOperation(IL_stloc_s,loc) );
        else
            d_body.append(IlOperation(IL_stloc,loc) );
   }
    delta(-1);
}

void IlEmitter::stobj_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_stobj,typeRef) );
    delta(-2);
}

void IlEmitter::stsfld_(const QByteArray& fieldRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_stsfld,fieldRef) );
    delta(-1);
}

void IlEmitter::sub_(bool withOverflow, bool withUnsignedOverflow)
{
    Q_ASSERT( !d_method.isEmpty() );
    if( withUnsignedOverflow )
        d_body.append(IlOperation(IL_sub_ovf_un ) );
    else if( withOverflow )
        d_body.append(IlOperation(IL_sub_ovf ) );
    else
        d_body.append(IlOperation(IL_sub ) );
    delta(-2+1);
}

void IlEmitter::throw_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_throw) );
    delta(-1);
}

void IlEmitter::unbox_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_unbox,typeRef) );
    delta(-1+1);
}

void IlEmitter::xor_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(IlOperation(IL_xor) );
    delta(-2+1);
}

void IlEmitter::delta(int d)
{
    int s = d_stackDepth;
    s += d;
   // TODO Q_ASSERT( s >= 0 );
    d_stackDepth = s;
    if( d_stackDepth > d_maxStackDepth )
        d_maxStackDepth = d_stackDepth;
}

static const char* s_opName[] =
{
    "IL_invalid",
    "IL_label",
    "IL_comment",
    "IL_line",
    "<not used>",
    "add", "add.ovf", "add.ovf.un", "and", "arglist", "beq", "beq.s", "bge",
    "bge.s", "bge.un", "bge.un.s", "bgt", "bgt.s", "bgt.un", "bgt.un.s", "ble",
    "ble.s", "ble.un", "ble.un.s", "blt", "blt.s", "blt.un", "blt.un.s", "bne.un",
    "bne.un.s", "box", "br", "br.s", "break", "brfalse", "brfalse.s", "brinst",
    "brinst.s", "brnull", "brnull.s", "brtrue", "brtrue.s", "brzero", "brzero.s", "call",
    "calli", "callvirt", "castclass", "ceq", "cgt", "cgt.un", "ckfinite", "clt",
    "clt.un", "constrained.", "conv.i", "conv.i1", "conv.i2", "conv.i4", "conv.i8", "conv.ovf.i",
    "conv.ovf.i.un", "conv.ovf.i1", "conv.ovf.i1.un", "conv.ovf.i2", "conv.ovf.i2.un", "conv.ovf.i4", "conv.ovf.i4.un", "conv.ovf.i8",
    "conv.ovf.i8.un", "conv.ovf.u", "conv.ovf.u.un", "conv.ovf.u1", "conv.ovf.u1.un", "conv.ovf.u2", "conv.ovf.u2.un", "conv.ovf.u4",
    "conv.ovf.u4.un", "conv.ovf.u8", "conv.ovf.u8.un", "conv.r.un", "conv.r4", "conv.r8", "conv.u", "conv.u1",
    "conv.u2", "conv.u4", "conv.u8", "cpblk", "cpobj", "div", "div.un", "dup",
    "endfault", "endfilter", "endfinally", "initblk", "initobj", "isinst", "jmp", "ldarg",
    "ldarg.0", "ldarg.1", "ldarg.2", "ldarg.3", "ldarg.s", "ldarga", "ldarga.s", "ldc.i4",
    "ldc.i4.0", "ldc.i4.1", "ldc.i4.2", "ldc.i4.3", "ldc.i4.4", "ldc.i4.5", "ldc.i4.6", "ldc.i4.7",
    "ldc.i4.8", "ldc.i4.m1", "ldc.i4.M1", "ldc.i4.s", "ldc.i8", "ldc.r4", "ldc.r8", "ldelem",
    "ldelem.i", "ldelem.i1", "ldelem.i2", "ldelem.i4", "ldelem.i8", "ldelem.r4", "ldelem.r8", "ldelem.ref",
    "ldelem.u1", "ldelem.u2", "ldelem.u4", "ldelem.u8", "ldelema", "ldfld", "ldflda", "ldftn",
    "ldind.i", "ldind.i1", "ldind.i2", "ldind.i4", "ldind.i8", "ldind.r4", "ldind.r8", "ldind.ref",
    "ldind.u1", "ldind.u2", "ldind.u4", "ldind.u8", "ldlen", "ldloc", "ldloc.0", "ldloc.1",
    "ldloc.2", "ldloc.3", "ldloc.s", "ldloca", "ldloca.s", "ldnull", "ldobj", "ldsfld",
    "ldsflda", "ldstr", "ldtoken", "ldvirtftn", "leave", "leave.s", "localloc", "mkrefany",
    "mul", "mul.ovf", "mul.ovf.un", "neg", "newarr", "newobj", "no.", "nop",
    "not", "or", "pop", "readonly.", "refanytype", "refanyval", "rem", "rem.un",
    "ret", "rethrow", "shl", "shr", "shr.un", "sizeof", "starg", "starg.s",
    "stelem", "stelem.i", "stelem.i1", "stelem.i2", "stelem.i4", "stelem.i8", "stelem.r4", "stelem.r8",
    "stelem.ref", "stfld", "stind.i", "stind.i1", "stind.i2", "stind.i4", "stind.i8", "stind.r4",
    "stind.r8", "stind.ref", "stloc", "stloc.0", "stloc.1", "stloc.2", "stloc.3", "stloc.s",
    "stobj", "stsfld", "sub", "sub.ovf", "sub.ovf.un", "switch", "tail.", "throw",
    "unaligned.", "unbox", "unbox.any", "volatile.", "xor"
};

IlAsmRenderer::IlAsmRenderer(QIODevice* dev):level(0),sourceRendered(false)
{
    // default is UTF-8, so no need to setCodec
    out.setDevice(dev);
}

void IlAsmRenderer::beginModule(const QByteArray& assemblyName, const QByteArray& moduleName, const QByteArrayList& imports, const QString& sourceFile, quint8 moduleKind)
{
    levels.push_back(moduleName);
    source = sourceFile;
    sourceRendered = false;
    out << "// Generated by " << qApp->applicationName() << " " << qApp->applicationVersion() << " on "
           << QDateTime::currentDateTime().toString(Qt::ISODate) << endl << endl;

#if 0
    out << ".language 'Oberon+'" << endl;
    out << ".line 1 '" << source << "'" << endl << endl;
#endif

    out << ".assembly " << assemblyName << " {}" << endl;
    QByteArray name = assemblyName;
    if( name.startsWith('\'') )
        name = name.mid(1, name.size() - 2 );

    out << ".module '" << name;
    if( moduleKind == IlEmitter::Library )
        out << ".dll'";
    else
        out << ".exe'";
    out << endl << endl;

    foreach( const QByteArray& imp, imports )
        out << ".assembly extern " << imp << " {}" << endl;

    if( !imports.isEmpty() )
        out << endl;

#if 0
    out << ".class public sealed " << moduleName
        // << formatMetaParams(me)
        << " extends [mscorlib]System.ValueType {" << endl; // MODULE
#else
    out << ".class public " << moduleName
        << " extends [mscorlib]System.Object {" << endl; // MODULE
#endif
    level++;
}

void IlAsmRenderer::endModule()
{
    level--;
    out << ws() << "}" << endl;
    levels.pop_back();
}

void IlAsmRenderer::addMethod(const IlMethod& m)
{
    out << ws() << ".method ";

    if( true ) // m.d_isPublic )
        out << "public ";
    else
        out << "assembly ";

    switch(m.d_methodKind)
    {
    case IlEmitter::Static:
    case IlEmitter::Primary:
        out << "static final ";
        break;
    case IlEmitter::Pinvoke:
        out << "static pinvokeimpl(";
        if( !m.d_library.isEmpty() )
            out << "\"" << m.d_library << "\"";
        if( !m.d_origName.isEmpty() )
            out << " as \"" << m.d_origName << "\"";
        out << " cdecl) ";
        break;
    case IlEmitter::Virtual:
        out << "virtual "; // a virtual instance method
        break;
    case IlEmitter::Instance:
        //out << "instance "; // instance is default; an instance method is either virtual or nonvirtual
        break;
    default:
        Q_ASSERT(false);
    }

    if( m.d_name == ".ctor" || m.d_name == ".cctor" )
        out << "specialname rtspecialname ";

    if( m.d_isVararg )
        out << "vararg ";

    if( m.d_retType.isEmpty() )
        out << "void";
    else
        out << m.d_retType;

    out << " /* " << levels.join('.') << " */ " << m.d_name << "(";

    for( int i = 0; i < m.d_args.size(); i++ )
    {
        if( i != 0 )
            out << ", ";
        out << m.d_args[i].first;
        if( !m.d_args[i].second.isEmpty() )
            out << " " << m.d_args[i].second;
    }

    out << ") ";
    if( m.d_isRuntime )
        out << "runtime ";
    else
        out << "cil ";
    out << "managed {";
    if( m.d_isRuntime || m.d_methodKind == IlEmitter::Pinvoke )
    {
        out << "}" << endl;
        return;
    }else
        out << endl;
    level++;
    out << ws() << ".maxstack " << m.d_stackDepth << endl;
    if( m.d_methodKind == IlEmitter::Primary )
        out << ws() << ".entrypoint" << endl;

    if( !m.d_locals.isEmpty() )
    {
        out << ws() << ".locals init (";
        level++;
        for( int i = 0; i < m.d_locals.size(); i++ )
        {
            if( i != 0 )
                out << ", " << endl << ws();
            out << "[" << i << "] " << m.d_locals[i].first;
            if( !m.d_locals[i].second.isEmpty() )
                out << " " << m.d_locals[i].second;
        }
        out << ")" << endl;
        level--;
    }

    for( int i = 0; i < m.d_body.size(); i++ )
    {
        const IlOperation& op = m.d_body[i];
        switch( op.d_ilop )
        {
        case IL_invalid:
            break;
        case IL_label:
            out << "'#" << op.d_arg << "':" << endl;
            break;
        case IL_line:
            if( i+1 < m.d_body.size() && m.d_body[i+1].d_ilop != IL_label )
            {
                out << ws() << ".line " << op.d_arg;
                if( !sourceRendered )
                {
                    out << " '" << source << "'";
                    sourceRendered = true;
                }
                out << endl;
            }
            break;
        case IL_brinst:
        case IL_brtrue:
        case IL_brzero:
        case IL_brnull:
        case IL_brfalse:
        case IL_br:
        case IL_bne_un:
        case IL_blt:
        case IL_blt_un:
        case IL_ble:
        case IL_ble_un:
        case IL_bgt:
        case IL_bgt_un:
        case IL_bge:
        case IL_bge_un:
        case IL_beq:
            out << ws() << s_opName[op.d_ilop] << " '#" << op.d_arg << "'" << endl;
            break;
        case IL_call:
            out << ws() << s_opName[op.d_ilop];
            if( op.d_flags )
                out << " instance";
            out << " " << op.d_arg << endl;
            break;
        case IL_callvirt:
        case IL_newobj:
            out << ws() << s_opName[op.d_ilop] << " instance " << op.d_arg << endl;
            break;
        default:
            out << ws() << s_opName[op.d_ilop];
            if( !op.d_arg.isEmpty() )
                out << " " << op.d_arg;
            out << endl;
            break;
        }
    }

    level--;
    out << ws() << "}" << endl;
}

void IlAsmRenderer::beginClass(const QByteArray& className, bool isPublic, quint8 classKind, const QByteArray& superClassRef, int byteSize)
{
    levels.push_back(className);

    out << ws() << ".class nested ";
    if( true ) // just make everything public // isPublic )
        out << "public ";
    else
        out << "assembly ";

    if( classKind == IlEmitter::Value )
        out << "sealed explicit ansi ";
    else if( classKind == IlEmitter::Delegate )
        out << "sealed ";

    out << className;
    // << formatMetaParams(thisMod)
    out << " extends ";
    if( !superClassRef.isEmpty() )
        out << superClassRef;
    else if( classKind == IlEmitter::Value )
        out << "[mscorlib]System.ValueType";
    else if( classKind == IlEmitter::Delegate )
        out << "[mscorlib]System.MulticastDelegate";
    else
        out << "[mscorlib]System.Object";
    out << " {" << endl;
    level++;
    if( byteSize >= 0 )
        out << ws() << ".size " << byteSize << endl;
    if( classKind == IlEmitter::Value )
        out << ws() << ".pack 0" << endl;
}

void IlAsmRenderer::endClass()
{
    levels.pop_back();
    level--;
    out << ws() << "}" << endl;
}

void IlAsmRenderer::addField(const QByteArray& fieldName, const QByteArray& typeRef, bool isPublic, bool isStatic, int explicitOffset, const QByteArray& marshalAs)
{
    out << ws() << ".field ";
    if( explicitOffset >= 0 )
        out << "[" << explicitOffset << "] ";
    if( true ) // just make everything public: isPublic )
        out << "public ";
    else
        out << "assembly ";
    if( isStatic )
        out << "static ";
    out << typeRef;
    if( !marshalAs.isEmpty() )
        out << " marshal(" << marshalAs << ")";
    out << " " << fieldName << endl;
}
