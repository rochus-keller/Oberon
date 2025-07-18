// This file was automatically generated by EbnfStudio; don't modify it!
#include "ObSynTree.h"
using namespace Ob;

SynTree::SynTree(quint16 r, const Token& t ):d_tok(r){
	d_tok.d_lineNr = t.d_lineNr;
	d_tok.d_colNr = t.d_colNr;
	d_tok.d_sourcePath = t.d_sourcePath;
}

const char* SynTree::rToStr( quint16 r ) {
	switch(r) {
		case R_ActualParameters_: return "ActualParameters";
		case R_AddOperator: return "AddOperator";
		case R_ArrayType: return "ArrayType";
		case R_BaseType: return "BaseType";
		case R_Case: return "Case";
		case R_CaseLabelList: return "CaseLabelList";
		case R_CaseStatement: return "CaseStatement";
		case R_ConstDeclaration: return "ConstDeclaration";
		case R_DeclarationSequence: return "DeclarationSequence";
		case R_DeclarationSequence2: return "DeclarationSequence2";
		case R_ElseStatement: return "ElseStatement";
		case R_ElsifStatement: return "ElsifStatement";
		case R_ElsifStatement2: return "ElsifStatement2";
		case R_ExitStatement: return "ExitStatement";
		case R_ExpList: return "ExpList";
		case R_FPSection: return "FPSection";
		case R_FieldList: return "FieldList";
		case R_FieldListSequence: return "FieldListSequence";
		case R_ForStatement: return "ForStatement";
		case R_FormalParameters: return "FormalParameters";
		case R_FormalType: return "FormalType";
		case R_Guard: return "Guard";
		case R_IdentList: return "IdentList";
		case R_IfStatement: return "IfStatement";
		case R_ImportList: return "ImportList";
		case R_LabelRange: return "LabelRange";
		case R_LengthList: return "LengthList";
		case R_LoopStatement: return "LoopStatement";
		case R_MulOperator: return "MulOperator";
		case R_NamedType: return "NamedType";
		case R_Oberon: return "Oberon";
		case R_PointerType: return "PointerType";
		case R_ProcedureBody: return "ProcedureBody";
		case R_ProcedureCall_: return "ProcedureCall";
		case R_ProcedureDeclaration: return "ProcedureDeclaration";
		case R_ProcedureHeading: return "ProcedureHeading";
		case R_ProcedureType: return "ProcedureType";
		case R_Receiver: return "Receiver";
		case R_RecordType: return "RecordType";
		case R_RepeatStatement: return "RepeatStatement";
		case R_ReturnStatement: return "ReturnStatement";
		case R_SimpleExpression: return "SimpleExpression";
		case R_StatementSequence: return "StatementSequence";
		case R_SysFlag: return "SysFlag";
		case R_TypeDeclaration: return "TypeDeclaration";
		case R_VariableDeclaration: return "VariableDeclaration";
		case R_WhileStatement: return "WhileStatement";
		case R_WithStatement: return "WithStatement";
		case R_assignment_: return "assignment";
		case R_assignmentOrProcedureCall: return "assignmentOrProcedureCall";
		case R_comment_: return "comment";
		case R_definition: return "definition";
		case R_designator: return "designator";
		case R_directive_: return "directive";
		case R_element: return "element";
		case R_expression: return "expression";
		case R_factor: return "factor";
		case R_hexstringdelim_: return "hexstringdelim";
		case R_identdef: return "identdef";
		case R_import: return "import";
		case R_label: return "label";
		case R_literal: return "literal";
		case R_module: return "module";
		case R_number: return "number";
		case R_obnx_: return "obnx";
		case R_qualident: return "qualident";
		case R_relation: return "relation";
		case R_selector: return "selector";
		case R_set: return "set";
		case R_statement: return "statement";
		case R_term: return "term";
		case R_type: return "type";
		case R_variableOrFunctionCall: return "variableOrFunctionCall";
	default: if(r<R_First) return tokenTypeName(r); else return "";
}
}
