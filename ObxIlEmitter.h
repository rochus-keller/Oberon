#ifndef OBXILEMITTER_H
#define OBXILEMITTER_H

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

#include <QByteArray>
#include <QPair>
#include <QList>
#include <QIODevice>
#include <QTextStream>

namespace Ob
{
    class RowCol;
}

namespace Obx
{
    struct IlOperation
    {
        uint d_ilop : 8;
        uint d_flags : 24;
        QByteArray d_arg;
        IlOperation(quint8 ilop = 0):d_ilop(ilop),d_flags(0){}
        IlOperation(quint8 ilop, quint32 numArg):d_ilop(ilop),d_arg(QByteArray::number(numArg)){}
        IlOperation(quint8 ilop, const QByteArray& strArg, quint16 flags = 0 ):d_ilop(ilop),d_flags(flags),d_arg(strArg){}
    };

    struct IlMethod
    {
        uint d_methodKind : 3;
        uint d_isPublic : 1;
        uint d_isRuntime : 1;
        uint d_isVararg : 1;
        uint d_stackDepth: 16;
        QByteArray d_name;
        QList<IlOperation> d_body;
        QList< QPair<QByteArray,QByteArray> > d_args; // idx, type, name
        QList< QPair<QByteArray,QByteArray> > d_locals; // idx, type, name
        QByteArray d_retType;
        QByteArray d_library, d_origName; // pinvoke
    };

    class IlRenderer
    {
    public:
        virtual void beginModule( const QByteArray& assemblyName, const QByteArray& moduleName,
                                  const QByteArrayList& imports, const QString& sourceFile, quint8 moduleKind ) {}
        virtual void endModule() {}

        virtual void addMethod(const IlMethod& method ) {}

        virtual void beginClass(const QByteArray& className, bool isPublic, quint8 classKind,
                         const QByteArray& superClassRef = QByteArray(), int byteSize = -1 ) {}
        virtual void endClass() {}

        virtual void addField( const QByteArray& fieldName, // on top level or in class
                       const QByteArray& typeRef,
                       bool isPublic = true,
                       bool isStatic = false,
                       int explicitOffset = -1,
                       const QByteArray& marshalAs = QByteArray() ) {}
        // marshalAs syntax like ILASM nativeType production, i.e. the contents of marshal() predicate,
        // e.g. "fixed array [128]", see ISO 23271 p. 502 (526)
    };

    class IlEmitter
    {
    public:
        IlEmitter(IlRenderer*);

        enum ModuleKind { Library, // no .subsystem
                            GuiApp, // .subsystem = 2
                            ConsoleApp, // .subsystem = 3
                          };

        void beginModule( const QByteArray& assemblyName, const QByteArray& moduleName, const QByteArrayList& imports,
                          const QString& sourceFile, ModuleKind = Library );
        void endModule();

        enum MethodKind { Static, Primary, Instance, Virtual, Pinvoke };
        void beginMethod(const QByteArray& methodName, // can be on top level or in a class/struct; cannot be in a method
                         bool isPublic = true,
                         MethodKind = Instance,
                         bool isRuntime = false ); // runtime used for delegates
        void endMethod();

        enum ClassKind { Object, Value, Delegate };

        void beginClass(const QByteArray& className, bool isPublic = true, quint8 classKind = Object,
                         const QByteArray& superClassRef = QByteArray(), int byteSize = -1 );
        void endClass(); // classes can be nested

        void addField( const QByteArray& fieldName, // on top level or in class
                       const QByteArray& typeRef,
                       bool isPublic = true,
                       bool isStatic = false,
                       int explicitOffset = -1,
                       const QByteArray& marshalAs = QByteArray() );

        quint32 addLocal( const QByteArray& typeRef, QByteArray name = QByteArray() );
        quint32 addArgument(const QByteArray& typeRef, QByteArray name = QByteArray() );
        void setReturnType(const QByteArray& typeRef);
        void setPinvoke( const QByteArray& lib, const QByteArray& origName = QByteArray() );
        void setVararg();

        quint32 newLabel();
        void label_(quint32); // inserts artificial label command
        void line_(const Ob::RowCol&);

        void add_( bool withOverflow = false, bool withUnsignedOverflow = false );
        void and_();
        void beq_( quint32 label );
        void bge_( quint32 label, bool withUnsigned = false );
        void bgt_( quint32 label, bool withUnsigned = false );
        void ble_( quint32 label, bool withUnsigned = false );
        void blt_( quint32 label, bool withUnsigned = false );
        void bne_( quint32 label );
        void box_(const QByteArray& typeRef);
        void br_( quint32 label );
        void break_();
        void brfalse_( quint32 label );
        void brnull_( quint32 label );
        void brzero_( quint32 label );
        void brtrue_( quint32 label );
        void brinst_( quint32 label );
        void call_( const QByteArray& methodRef, int argCount = 0, bool hasRet = false, bool isInstance = false );
        void callvirt_( const QByteArray& methodRef, int argCount, bool hasRet = false );
        void castclass_(const QByteArray& typeRef);
        void ceq_();
        void cgt_(bool withUnsigned = false);
        void clt_(bool withUnsigned = false);
        void cpblk_();
        enum ToType { ToI1, ToI2, ToI4, ToI8, ToR4, ToR8, ToU1, ToU2, ToU4, ToU8, ToI };
        void conv_( ToType, bool withOverflow = false, bool withUnsignedOverflow = false );
        void div_(bool withUnsigned = false);
        void dup_();
        void initblk_();
        void initobj_(const QByteArray& typeRef);
        void isinst_(const QByteArray& typeRef);
        void ldarg_(quint16 arg);
        void ldarga_(quint16 arg);
        void ldc_i4(qint32);
        void ldc_i8(qint64);
        void ldc_r4(double);
        void ldc_r8(double);
        void ldelem_(const QByteArray& typeRef);
        void ldelema_(const QByteArray& typeRef);
        void ldfld_(const QByteArray& fieldRef);
        void ldflda_(const QByteArray& fieldRef);
        void ldftn_(const QByteArray& methodRef);
        enum IndType { I1, I2, I4, I8, R4, R8, Ref, U1, U2, U4, U8, IntPtr };
        void ldind_(IndType);
        void ldlen_();
        void ldloc_(quint16);
        void ldloca_(quint16);
        void ldnull_();
        void ldobj_(const QByteArray& typeRef);
        void ldsfld_(const QByteArray& fieldRef);
        void ldsflda_(const QByteArray& fieldRef);
        void ldstr_(const QByteArray& utf8);
        void ldvirtftn_(const QByteArray& methodRef);
        void localloc_();
        void mul_(bool withOverflow = false, bool withUnsignedOverflow = false);
        void neg_();
        void newarr_(const QByteArray& typeRef);
        void newobj_(const QByteArray& methodRef, int argCount = 0);
        void nop_();
        void not_();
        void or_();
        void pop_();
        void rem_( bool withUnsigned = false );
        void ret_(bool hasRetType = false);
        void shl_();
        void shr_(bool withUnsigned = false );
        void starg_(quint16);
        void stelem_(const QByteArray& typeRef);
        void stfld_(const QByteArray& fieldRef);
        void stind_( IndType ); // without Ux
        void stloc_(quint16);
        void stobj_(const QByteArray& typeRef);
        void stsfld_(const QByteArray& fieldRef);
        void sub_( bool withOverflow = false, bool withUnsignedOverflow = false );
        void throw_();
        void unbox_(const QByteArray& typeRef);
        void xor_();
    protected:
        void delta(int d);
    private:
        quint8 d_methodKind;
        bool d_isPublic;
        bool d_isRuntime;
        bool d_isVararg;
        quint32 d_labelCount;
        quint16 d_stackDepth;
        quint16 d_maxStackDepth;
        QByteArray d_method;
        QList<IlOperation> d_body;
        QList< QPair<QByteArray,QByteArray> > d_args; // idx, type, name
        QList< QPair<QByteArray,QByteArray> > d_locals; // idx, type, name
        QByteArray d_retType;
        QByteArray d_library, d_origName;
        IlRenderer* d_out;
    };

    enum IL_op
    {
        IL_invalid,
        IL_label,
        IL_comment,
        IL_line, // row ':' column
        IL_unused, // to sync with DotNetPELib::Instruction::iop
        IL_add, IL_add_ovf, IL_add_ovf_un, IL_and, IL_arglist, IL_beq, IL_beq_s, IL_bge,
        IL_bge_s, IL_bge_un, IL_bge_un_s, IL_bgt, IL_bgt_s, IL_bgt_un, IL_bgt_un_s, IL_ble,
        IL_ble_s, IL_ble_un, IL_ble_un_s, IL_blt, IL_blt_s, IL_blt_un, IL_blt_un_s, IL_bne_un,
        IL_bne_un_s, IL_box, IL_br, IL_br_s, IL_break, IL_brfalse, IL_brfalse_s, IL_brinst,
        IL_brinst_s, IL_brnull, IL_brnull_s, IL_brtrue, IL_brtrue_s, IL_brzero, IL_brzero_s, IL_call,
        IL_calli, IL_callvirt, IL_castclass, IL_ceq, IL_cgt, IL_cgt_un, IL_ckfinite, IL_clt,
        IL_clt_un, IL_constrained_, IL_conv_i, IL_conv_i1, IL_conv_i2, IL_conv_i4, IL_conv_i8, IL_conv_ovf_i,
        IL_conv_ovf_i_un, IL_conv_ovf_i1, IL_conv_ovf_i1_un, IL_conv_ovf_i2, IL_conv_ovf_i2_un, IL_conv_ovf_i4, IL_conv_ovf_i4_un, IL_conv_ovf_i8,
        IL_conv_ovf_i8_un, IL_conv_ovf_u, IL_conv_ovf_u_un, IL_conv_ovf_u1, IL_conv_ovf_u1_un, IL_conv_ovf_u2, IL_conv_ovf_u2_un, IL_conv_ovf_u4,
        IL_conv_ovf_u4_un, IL_conv_ovf_u8, IL_conv_ovf_u8_un, IL_conv_r_un, IL_conv_r4, IL_conv_r8, IL_conv_u, IL_conv_u1,
        IL_conv_u2, IL_conv_u4, IL_conv_u8, IL_cpblk, IL_cpobj, IL_div, IL_div_un, IL_dup,
        IL_endfault, IL_endfilter, IL_endfinally, IL_initblk, IL_initobj, IL_isinst, IL_jmp, IL_ldarg,
        IL_ldarg_0, IL_ldarg_1, IL_ldarg_2, IL_ldarg_3, IL_ldarg_s, IL_ldarga, IL_ldarga_s, IL_ldc_i4,
        IL_ldc_i4_0, IL_ldc_i4_1, IL_ldc_i4_2, IL_ldc_i4_3, IL_ldc_i4_4, IL_ldc_i4_5, IL_ldc_i4_6, IL_ldc_i4_7,
        IL_ldc_i4_8, IL_ldc_i4_m1, IL_ldc_i4_M1, IL_ldc_i4_s, IL_ldc_i8, IL_ldc_r4, IL_ldc_r8, IL_ldelem,
        IL_ldelem_i, IL_ldelem_i1, IL_ldelem_i2, IL_ldelem_i4, IL_ldelem_i8, IL_ldelem_r4, IL_ldelem_r8, IL_ldelem_ref,
        IL_ldelem_u1, IL_ldelem_u2, IL_ldelem_u4, IL_ldelem_u8, IL_ldelema, IL_ldfld, IL_ldflda, IL_ldftn,
        IL_ldind_i, IL_ldind_i1, IL_ldind_i2, IL_ldind_i4, IL_ldind_i8, IL_ldind_r4, IL_ldind_r8, IL_ldind_ref,
        IL_ldind_u1, IL_ldind_u2, IL_ldind_u4, IL_ldind_u8, IL_ldlen, IL_ldloc, IL_ldloc_0, IL_ldloc_1,
        IL_ldloc_2, IL_ldloc_3, IL_ldloc_s, IL_ldloca, IL_ldloca_s, IL_ldnull, IL_ldobj, IL_ldsfld,
        IL_ldsflda, IL_ldstr, IL_ldtoken, IL_ldvirtftn, IL_leave, IL_leave_s, IL_localloc, IL_mkrefany,
        IL_mul, IL_mul_ovf, IL_mul_ovf_un, IL_neg, IL_newarr, IL_newobj, IL_no_, IL_nop,
        IL_not, IL_or, IL_pop, IL_readonly_, IL_refanytype, IL_refanyval, IL_rem, IL_rem_un,
        IL_ret, IL_rethrow, IL_shl, IL_shr, IL_shr_un, IL_sizeof, IL_starg, IL_starg_s,
        IL_stelem, IL_stelem_i, IL_stelem_i1, IL_stelem_i2, IL_stelem_i4, IL_stelem_i8, IL_stelem_r4, IL_stelem_r8,
        IL_stelem_ref, IL_stfld, IL_stind_i, IL_stind_i1, IL_stind_i2, IL_stind_i4, IL_stind_i8, IL_stind_r4,
        IL_stind_r8, IL_stind_ref, IL_stloc, IL_stloc_0, IL_stloc_1, IL_stloc_2, IL_stloc_3, IL_stloc_s,
        IL_stobj, IL_stsfld, IL_sub, IL_sub_ovf, IL_sub_ovf_un, IL_switch, IL_tail_, IL_throw,
        IL_unaligned_, IL_unbox, IL_unbox_any, IL_volatile_, IL_xor
    };

    class IlAsmRenderer : public IlRenderer
    {
    public:
        IlAsmRenderer(QIODevice*);

        virtual void beginModule(const QByteArray& assemblyName, const QByteArray& moduleName,
                                 const QByteArrayList& imports, const QString& sourceFile, quint8 moduleKind );
        virtual void endModule();

        virtual void addMethod(const IlMethod& method );

        virtual void beginClass(const QByteArray& className, bool isPublic, quint8 classKind,
                         const QByteArray& superClassRef = QByteArray(), int byteSize = -1 );
        virtual void endClass();

        virtual void addField( const QByteArray& fieldName,
                       const QByteArray& typeRef,
                       bool isPublic = true,
                       bool isStatic = false,
                       int explicitOffset = -1,
                       const QByteArray& marshalAs = QByteArray() );
    protected:
        inline QByteArray ws() { return QByteArray(level*4,' '); }
    private:
        QTextStream out;
        int level;
        QString source;
        QByteArrayList levels;
        bool sourceRendered;
    };

}

#endif // OBXILEMITTER_H
