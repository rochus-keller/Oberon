#ifndef __OB_SYNTREE__
#define __OB_SYNTREE__
// This file was automatically generated by EbnfStudio; don't modify it!

#include <Oberon/ObTokenType.h>
#include <Oberon/ObToken.h>
#include <QList>

namespace Ob {

	struct SynTree {
		enum ParserRule {
			R_First = TT_Max + 1,
			R_ActualParameters_,
			R_AddOperator,
			R_ArrayType,
			R_BaseType,
			R_Case,
			R_CaseLabelList,
			R_CaseStatement,
			R_ConstDeclaration,
			R_DeclarationSequence,
			R_DeclarationSequence2,
			R_ElseStatement,
			R_ElsifStatement,
			R_ElsifStatement2,
			R_ExitStatement,
			R_ExpList,
			R_FPSection,
			R_FieldList,
			R_FieldListSequence,
			R_ForStatement,
			R_FormalParameters,
			R_FormalType,
			R_Guard,
			R_IdentList,
			R_IfStatement,
			R_ImportList,
			R_LabelRange,
			R_LengthList,
			R_LoopStatement,
			R_MulOperator,
			R_NamedType,
			R_Oberon,
			R_PointerType,
			R_ProcedureBody,
			R_ProcedureCall_,
			R_ProcedureDeclaration,
			R_ProcedureHeading,
			R_ProcedureType,
			R_Receiver,
			R_RecordType,
			R_RepeatStatement,
			R_ReturnStatement,
			R_SimpleExpression,
			R_StatementSequence,
			R_TypeDeclaration,
			R_VariableDeclaration,
			R_WhileStatement,
			R_WithStatement,
			R_assignment_,
			R_assignmentOrProcedureCall,
			R_comment_,
			R_definition,
			R_designator,
			R_element,
			R_expression,
			R_factor,
			R_identdef,
			R_import,
			R_label,
			R_literal,
			R_module,
			R_number,
			R_obnx_,
			R_qualident,
			R_relation,
			R_selector,
			R_set,
			R_statement,
			R_term,
			R_type,
			R_variableOrFunctionCall,
			R_Last
		};
		SynTree(quint16 r = Tok_Invalid, const Token& = Token() );
		SynTree(const Token& t ):d_tok(t){}
		~SynTree() { foreach(SynTree* n, d_children) delete n; }

		static const char* rToStr( quint16 r );

		Ob::Token d_tok;
		QList<SynTree*> d_children;
	};

}
#endif // __OB_SYNTREE__
