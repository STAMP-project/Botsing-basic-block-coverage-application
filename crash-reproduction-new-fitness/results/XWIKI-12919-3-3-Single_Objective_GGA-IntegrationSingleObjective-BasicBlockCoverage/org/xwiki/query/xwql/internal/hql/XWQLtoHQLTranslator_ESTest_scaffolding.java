/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Sun May 17 10:49:10 UTC 2020
 */

package org.xwiki.query.xwql.internal.hql;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

import static org.evosuite.shaded.org.mockito.Mockito.*;
@EvoSuiteClassExclude
public class XWQLtoHQLTranslator_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.xwiki.query.xwql.internal.hql.XWQLtoHQLTranslator"; 
    org.evosuite.runtime.GuiSupport.initialize(); 
    org.evosuite.runtime.RuntimeSettings.maxNumberOfIterationsPerLoop = 10000; 
    org.evosuite.runtime.RuntimeSettings.mockSystemIn = true; 
    org.evosuite.runtime.Runtime.getInstance().resetRuntime(); 
    try { initMocksToAvoidTimeoutsInTheTests(); } catch(ClassNotFoundException e) {} 
  } 

  @Before 
  public void initTestCase(){ 
    threadStopper.storeCurrentThreads();
    threadStopper.startRecordingTime();
    org.evosuite.runtime.GuiSupport.setHeadless(); 
    org.evosuite.runtime.Runtime.getInstance().resetRuntime(); 
    org.evosuite.runtime.agent.InstrumentingAgent.activate(); 
  } 

  @After 
  public void doneWithTestCase(){ 
    threadStopper.killAndJoinClientThreads();
    org.evosuite.runtime.agent.InstrumentingAgent.deactivate(); 
    org.evosuite.runtime.GuiSupport.restoreHeadlessMode(); 
  } 


  private static void initializeClasses() {
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(XWQLtoHQLTranslator_ESTest_scaffolding.class.getClassLoader() ,
      "com.xpn.xwiki.XWikiException",
      "org.xwiki.query.jpql.node.Token",
      "org.xwiki.query.jpql.node.AConditionalFactor",
      "org.xwiki.query.jpql.node.APathSimpleSelectExpression",
      "org.xwiki.query.jpql.node.PIsExpression",
      "org.xwiki.query.jpql.node.TLength",
      "org.xwiki.query.jpql.node.PSelectList",
      "org.apache.commons.lang3.StringUtils",
      "org.xwiki.query.jpql.node.AFunctimeArithmeticPrimary",
      "org.xwiki.query.jpql.node.APathCountArg",
      "org.xwiki.query.jpql.node.PComparisonExpression",
      "org.xwiki.query.jpql.node.TEmpty",
      "org.xwiki.query.jpql.node.PFromList",
      "org.xwiki.query.jpql.node.TBy",
      "org.xwiki.query.jpql.node.ANullIsExpression",
      "org.xwiki.query.jpql.node.AInConditionWithNotExpression",
      "org.xwiki.query.jpql.node.PJoinSpec",
      "org.xwiki.query.jpql.node.PSimpleSelectExpression",
      "org.xwiki.query.jpql.node.ABetweenExpression",
      "org.xwiki.query.jpql.node.TLocate",
      "org.xwiki.query.jpql.node.TSelect",
      "org.xwiki.query.jpql.node.PGroupbyClause",
      "org.xwiki.query.jpql.node.ABoolLiteral",
      "org.xwiki.query.jpql.node.PConditionalPrimary",
      "org.xwiki.query.jpql.node.ANumLiteral",
      "org.xwiki.query.xwql.internal.QueryContext$PropertyInfo",
      "org.xwiki.query.xwql.internal.QueryAnalyzer",
      "org.xwiki.query.xwql.internal.hql.XWQLtoHQLTranslator",
      "org.xwiki.query.jpql.node.AObjectSelectExpression",
      "org.xwiki.query.jpql.node.ASingleSelectList",
      "org.xwiki.model.internal.reference.DefaultStringEntityReferenceSerializer",
      "org.xwiki.context.ExecutionContext",
      "org.xwiki.query.jpql.analysis.AnalysisAdapter",
      "org.xwiki.query.jpql.node.PAllanyExpression",
      "org.xwiki.query.jpql.node.PSubquery",
      "org.xwiki.query.jpql.node.ABetweenConditionWithNotExpression",
      "org.xwiki.query.jpql.node.TMember",
      "org.xwiki.query.jpql.node.TWhite",
      "org.xwiki.query.jpql.node.AAllanyExpression",
      "org.xwiki.query.jpql.node.AAsSubselectIdentificationVariableDeclaration",
      "org.xwiki.query.jpql.node.PJoin",
      "org.xwiki.query.jpql.node.APathSelectExpression",
      "org.xwiki.query.jpql.node.ASelectList",
      "org.xwiki.query.jpql.node.ALengthFunctionsReturningNumerics",
      "org.xwiki.query.xwql.internal.QueryContext",
      "org.xwiki.query.jpql.node.TLike",
      "org.jfree.ui.FilesystemFilter",
      "org.xwiki.query.jpql.node.AOrderbyItem",
      "org.xwiki.query.jpql.node.AGroupbyList",
      "org.xwiki.query.jpql.node.AEmptyIsExpression",
      "org.xwiki.query.jpql.node.AXAbstractSchemaName",
      "org.xwiki.query.jpql.node.TInputParameter",
      "org.xwiki.query.jpql.node.AJoin",
      "org.xwiki.query.jpql.node.ASubstringFunctionsReturningStrings",
      "org.xwiki.query.jpql.node.ATrimCharacter",
      "org.xwiki.query.jpql.node.TNull",
      "org.xwiki.query.jpql.node.TExists",
      "org.xwiki.query.jpql.node.AEscapeSpec",
      "org.xwiki.query.jpql.node.TCount",
      "org.xwiki.query.jpql.node.TDot",
      "org.xwiki.query.jpql.node.PSimpleSelectClause",
      "org.xwiki.query.jpql.node.PInItem",
      "org.xwiki.query.jpql.node.PConditionalExpression",
      "org.xwiki.query.jpql.node.ASingleSimpleArithmeticExpression",
      "info.informatica.io.FilesystemInfo$TokenizedPath",
      "org.xwiki.query.jpql.node.AModFunctionsReturningNumerics",
      "org.xwiki.query.jpql.node.PSimpleArithmeticExpression",
      "org.xwiki.query.jpql.node.AEscapeCharacter",
      "org.xwiki.query.jpql.node.ASimpleArithmeticExpression",
      "org.xwiki.query.jpql.node.ASingleGroupbyList",
      "org.xwiki.query.jpql.node.AUpperFunctionsReturningStrings",
      "org.xwiki.model.reference.WikiReference",
      "org.xwiki.query.jpql.node.TAs",
      "org.xwiki.query.jpql.node.TFrom",
      "org.xwiki.query.jpql.node.TOrderbySpec",
      "org.xwiki.context.internal.ExecutionContextProperty",
      "org.xwiki.query.jpql.node.POrderbyClause",
      "org.xwiki.query.jpql.node.ATrimFunctionsReturningStrings",
      "org.xwiki.query.jpql.node.PAbstractSchemaName",
      "org.xwiki.query.jpql.node.TBooleanLiteral",
      "org.xwiki.query.jpql.node.ABrConditionalPrimary",
      "org.xwiki.query.jpql.node.AStrLiteral",
      "org.xwiki.bridge.DocumentModelBridge",
      "org.xwiki.query.jpql.node.APathComparableItem",
      "org.xwiki.query.xwql.internal.QueryContext$DocumentInfo",
      "org.xwiki.model.reference.SpaceReference",
      "org.xwiki.query.jpql.node.AConstStringPrimary",
      "org.xwiki.query.jpql.node.AFuncnumComparableItem",
      "org.xwiki.query.jpql.node.APathArithmeticPrimary",
      "org.xwiki.component.annotation.Role",
      "org.xwiki.query.jpql.lexer.LexerException",
      "org.xwiki.query.jpql.node.TFloatLiteral",
      "org.xwiki.query.jpql.parser.State",
      "org.xwiki.query.jpql.node.POrderbyList",
      "org.xwiki.query.jpql.node.PSubqueryFromClause",
      "org.xwiki.query.jpql.node.ALeftJoinSpec",
      "org.xwiki.model.internal.reference.LocalizedStringEntityReferenceSerializer",
      "org.xwiki.query.jpql.node.PNumericLiteral",
      "org.xwiki.query.xwql.internal.QueryTranslator",
      "org.xwiki.query.jpql.node.ALowerFunctionsReturningStrings",
      "org.xwiki.query.jpql.analysis.DepthFirstAdapter",
      "org.xwiki.query.jpql.node.PRangeVariableDeclaration",
      "org.xwiki.query.jpql.node.PNullComparisonExpression",
      "org.xwiki.query.jpql.node.ANullComparisonExpression",
      "org.xwiki.query.jpql.node.PSimpleCondExpression",
      "org.xwiki.query.jpql.node.TComma",
      "org.xwiki.query.jpql.node.TMulDiv",
      "org.xwiki.query.jpql.node.PFunctionsReturningStrings",
      "org.xwiki.query.jpql.node.PConditionWithNotExpression",
      "org.xwiki.query.jpql.node.PCountExpression",
      "org.xwiki.query.jpql.node.AAgrSimpleSelectExpression",
      "org.xwiki.query.jpql.node.PEmptyCollectionComparisonExpression",
      "org.xwiki.query.jpql.node.AHavingClause",
      "org.xwiki.query.jpql.node.AAgrArithmeticPrimary",
      "org.xwiki.query.jpql.node.PConditionalFactor",
      "org.xwiki.query.jpql.node.THaving",
      "org.xwiki.query.jpql.node.PVariable",
      "org.xwiki.query.jpql.node.TSqrt",
      "org.xwiki.component.annotation.Component",
      "org.xwiki.query.jpql.node.PTrimDesc",
      "org.xwiki.query.jpql.node.AConditionalTerm",
      "org.xwiki.query.jpql.node.TAddSub",
      "org.xwiki.query.jpql.node.AInnerJoinSpec",
      "org.xwiki.query.jpql.node.AAgrFunctionsReturningNumerics",
      "org.xwiki.query.jpql.node.ALikeConditionWithNotExpression",
      "org.xwiki.query.jpql.node.ARangeVariableDeclaration",
      "org.xwiki.query.jpql.node.ALocateFunctionsReturningNumerics",
      "org.xwiki.model.reference.DocumentReferenceResolver",
      "org.xwiki.query.jpql.node.PSelectClause",
      "org.xwiki.query.jpql.node.ASelectStatement",
      "org.xwiki.query.jpql.node.TInner",
      "org.xwiki.query.jpql.node.TLbr",
      "org.xwiki.query.jpql.node.TComparisonOperator",
      "org.xwiki.query.jpql.node.ACmpSimpleConditionalExpressionRemainder",
      "org.xwiki.query.jpql.node.AOrderbyList",
      "org.xwiki.query.jpql.node.PStatement",
      "org.xwiki.query.jpql.node.ASingleArithmeticTerm",
      "org.xwiki.query.jpql.node.PFromClause",
      "org.xwiki.query.jpql.node.TTJoin",
      "org.xwiki.query.jpql.node.Switch",
      "info.informatica.io.FilePatternSpec",
      "org.xwiki.query.jpql.node.PAggregateExpression",
      "org.xwiki.query.jpql.node.PFunctionsReturningNumerics",
      "org.xwiki.query.jpql.node.ABrArithmeticPrimary",
      "org.xwiki.query.jpql.node.ACollectionMemberExpression",
      "org.xwiki.context.PropertyIsFinalException",
      "org.xwiki.query.jpql.analysis.Analysis",
      "org.xwiki.model.reference.ObjectReference",
      "org.xwiki.model.reference.AttachmentReference",
      "org.xwiki.query.jpql.node.AMemberSubselectIdentificationVariableDeclaration",
      "org.xwiki.query.jpql.node.PInExpression",
      "org.xwiki.query.jpql.node.PIdentificationVariableDeclaration",
      "info.informatica.io.FilesystemInfo",
      "org.xwiki.query.jpql.parser.ParserException",
      "org.xwiki.query.jpql.node.TAnd",
      "org.xwiki.query.jpql.node.TBetween",
      "org.xwiki.query.jpql.node.PPath",
      "org.xwiki.query.jpql.node.ASingleFromList",
      "org.xwiki.query.jpql.node.AAasComparisonExpressionRightOperand",
      "org.xwiki.query.jpql.node.AArithmeticExpression",
      "org.xwiki.query.jpql.node.PCollectionMemberExpression",
      "org.xwiki.query.jpql.node.AMemberConditionWithNotExpression",
      "org.xwiki.model.reference.EntityReference",
      "org.xwiki.query.jpql.node.TFunctionsReturningDatetime",
      "org.xwiki.query.jpql.node.PArithmeticFactor",
      "org.xwiki.query.jpql.node.ASimpleCondExpression",
      "org.xwiki.query.jpql.node.TConcat",
      "org.xwiki.query.jpql.node.AArithmeticTerm",
      "org.xwiki.query.jpql.node.TDistinct",
      "org.xwiki.query.jpql.node.AStartSpec",
      "org.xwiki.query.jpql.node.TAbs",
      "org.xwiki.query.jpql.node.PCollectionMemberDeclaration",
      "org.xwiki.query.jpql.node.AXPath",
      "org.xwiki.context.ExecutionContext$DeclarationBuilder",
      "org.xwiki.query.jpql.node.PEscapeCharacter",
      "org.xwiki.query.jpql.node.AFuncAggregateExpression",
      "org.xwiki.query.jpql.node.ALitInItem",
      "org.xwiki.model.EntityType",
      "org.xwiki.query.jpql.node.AArithmeticFactor",
      "org.xwiki.query.jpql.node.APathStringPrimary",
      "org.xwiki.query.jpql.node.AFromList",
      "org.xwiki.query.jpql.node.PPatternValue",
      "org.xwiki.query.jpql.node.APathSubselectIdentificationVariableDeclaration",
      "org.xwiki.query.jpql.node.EOF",
      "org.xwiki.query.jpql.node.AConditionalExpression",
      "org.xwiki.query.jpql.node.AFunctimeComparableItem",
      "org.xwiki.query.jpql.node.ASimpleSelectClause",
      "org.xwiki.query.jpql.node.AAbstractSchemaName",
      "org.xwiki.query.jpql.node.TGroup",
      "org.xwiki.query.jpql.node.ASingleConditionalExpression",
      "org.xwiki.query.jpql.node.PXClassName",
      "org.xwiki.query.jpql.node.ATrimDesc",
      "org.xwiki.query.jpql.node.TEscape",
      "org.xwiki.query.jpql.node.ANotSimpleConditionalExpressionRemainder",
      "org.xwiki.query.jpql.node.Switchable",
      "org.xwiki.query.jpql.node.AInExpression",
      "org.xwiki.query.jpql.node.ASubquery",
      "org.xwiki.query.jpql.node.AMemberFromList",
      "org.xwiki.query.jpql.node.AFuncstrArithmeticPrimary",
      "org.xwiki.query.jpql.node.TIntegerLiteral",
      "org.xwiki.stability.Unstable",
      "com.xpn.xwiki.doc.DefaultDocumentAccessBridge",
      "org.xwiki.query.jpql.node.PSelectStatement",
      "org.xwiki.query.jpql.node.ASingleConditionalTerm",
      "org.xwiki.query.jpql.node.PConditionalTerm",
      "org.xwiki.security.authorization.ContextualAuthorizationManager",
      "org.xwiki.query.jpql.node.PLiteral",
      "org.xwiki.query.jpql.node.ASelectClause",
      "org.xwiki.query.jpql.node.AOrderbyClause",
      "org.xwiki.query.jpql.node.AQuotedXClassName",
      "org.xwiki.query.jpql.node.TId",
      "org.xwiki.query.jpql.node.TTrimSpecification",
      "org.xwiki.query.jpql.node.PBetweenExpression",
      "org.xwiki.query.jpql.node.ASubqueryArithmeticExpression",
      "org.xwiki.query.jpql.node.TFetch",
      "org.xwiki.query.jpql.node.APath",
      "org.xwiki.context.Execution",
      "org.xwiki.query.jpql.node.PGroupbyList",
      "org.xwiki.query.jpql.node.AIsSimpleConditionalExpressionRemainder",
      "org.xwiki.query.jpql.node.TRbr",
      "org.xwiki.context.ExecutionContext$1",
      "org.xwiki.query.jpql.node.TIs",
      "org.xwiki.query.xwql.internal.AliasGenerator",
      "org.xwiki.query.jpql.node.TIn",
      "org.xwiki.query.jpql.node.TOrder",
      "org.xwiki.query.jpql.node.POrderbyItem",
      "org.xwiki.query.jpql.node.PXObjectDecl",
      "org.xwiki.query.jpql.node.AXClassName",
      "org.xwiki.query.jpql.node.TSubstring",
      "org.xwiki.bridge.DocumentAccessBridge",
      "info.informatica.io.WildcardFilter",
      "org.xwiki.query.jpql.parser.Parser",
      "org.xwiki.query.jpql.node.AXObjectDecl",
      "org.xwiki.model.reference.DocumentReference",
      "org.xwiki.query.jpql.node.PArithmeticTerm",
      "com.xpn.xwiki.XWikiContext",
      "org.xwiki.query.jpql.node.ACountAggregateExpression",
      "org.xwiki.query.jpql.node.AFuncstrComparableItem",
      "com.xpn.xwiki.doc.XWikiDocument",
      "org.xwiki.query.jpql.node.TWhere",
      "org.xwiki.query.jpql.node.AParamArithmeticPrimary",
      "org.xwiki.query.xwql.internal.hql.Printer",
      "org.xwiki.query.jpql.node.AComparisonExpression",
      "org.xwiki.query.jpql.node.AConcatFunctionsReturningStrings",
      "org.xwiki.query.jpql.node.ALikeExpression",
      "org.xwiki.query.jpql.node.PEscapeSpec",
      "org.xwiki.query.jpql.lexer.Lexer$State",
      "org.xwiki.query.xwql.internal.InvalidQueryException",
      "org.xwiki.model.internal.reference.AbstractStringEntityReferenceSerializer",
      "org.xwiki.query.jpql.node.PArithmeticPrimary",
      "org.xwiki.query.jpql.node.AIdentificationVariableDeclaration",
      "org.xwiki.query.jpql.node.AAbsFunctionsReturningNumerics",
      "org.xwiki.query.jpql.node.AConditionalPrimary",
      "org.xwiki.query.jpql.node.PComparableItem",
      "org.xwiki.query.jpql.node.AFromClause",
      "org.xwiki.query.jpql.node.AParamPatternValue",
      "org.xwiki.query.jpql.node.PWhereClause",
      "org.xwiki.query.jpql.node.AConstantArithmeticPrimary",
      "org.xwiki.query.jpql.node.Start",
      "org.xwiki.query.jpql.node.AStrPatternValue",
      "org.xwiki.query.jpql.node.TOf",
      "org.xwiki.query.jpql.node.TUpper",
      "org.xwiki.query.jpql.node.TOuter",
      "org.xwiki.query.jpql.node.PArithmeticExpression",
      "org.xwiki.query.jpql.node.PSimpleConditionalExpressionRemainder",
      "org.xwiki.model.reference.EntityReferenceSerializer",
      "org.xwiki.query.jpql.node.AGroupbyClause",
      "org.xwiki.query.jpql.node.PLikeExpression",
      "org.xwiki.query.jpql.node.AFuncnumArithmeticPrimary",
      "org.xwiki.query.jpql.analysis.ReversedDepthFirstAdapter",
      "org.xwiki.query.jpql.node.AAgrSelectExpression",
      "org.xwiki.query.jpql.node.AMathComparisonExpressionRightOperand",
      "org.xwiki.context.PropertyAlreadyExistsException",
      "org.xwiki.query.xwql.internal.hql.PropertyPrinter",
      "org.xwiki.query.jpql.node.TMod",
      "org.xwiki.query.jpql.node.ASingleInList",
      "org.xwiki.query.jpql.node.AInList",
      "org.xwiki.query.jpql.node.PExistsExpression",
      "org.xwiki.query.jpql.node.AWhereClause",
      "org.xwiki.query.jpql.node.AFuncStringPrimary",
      "org.xwiki.query.jpql.node.TAllAnySome",
      "org.xwiki.query.jpql.node.TOr",
      "org.xwiki.query.xwql.internal.hql.TreePrinter",
      "org.xwiki.model.reference.ObjectPropertyReference",
      "org.xwiki.query.jpql.node.PStartSpec",
      "org.xwiki.query.jpql.node.ASubqueryInList",
      "org.xwiki.query.jpql.node.PComparisonExpressionRightOperand",
      "org.xwiki.query.jpql.node.AExistsExpression",
      "org.xwiki.query.jpql.node.ASingleSubselectFromList",
      "org.xwiki.query.jpql.node.TStringLiteral",
      "org.xwiki.query.jpql.node.Node",
      "org.xwiki.query.jpql.node.PSubselectFromList",
      "org.xwiki.query.jpql.node.PCountArg",
      "org.xwiki.query.jpql.node.ASizeFunctionsReturningNumerics",
      "org.xwiki.query.jpql.node.ASubqueryFromClause",
      "org.xwiki.query.jpql.node.AVariable",
      "org.xwiki.query.jpql.node.PSubselectIdentificationVariableDeclaration",
      "org.xwiki.query.jpql.node.AFloatNumericLiteral",
      "org.xwiki.query.jpql.node.PGroupbyItem",
      "org.xwiki.query.jpql.node.AParamStringPrimary",
      "org.xwiki.query.jpql.node.TLeft",
      "org.xwiki.query.xwql.internal.hql.ObjectPrinter",
      "org.xwiki.model.reference.LocalDocumentReference",
      "org.xwiki.query.jpql.node.AIntegerNumericLiteral",
      "org.xwiki.query.jpql.node.TSize",
      "info.informatica.io.FilePatternSpec$FilePattern",
      "org.xwiki.query.jpql.node.ASubselectFromList",
      "org.xwiki.query.jpql.node.TObject",
      "org.xwiki.query.jpql.node.AStatement",
      "org.xwiki.query.jpql.node.PHavingClause",
      "org.xwiki.query.jpql.internal.JPQLParser",
      "org.xwiki.query.jpql.node.PStringPrimary",
      "org.xwiki.query.jpql.lexer.Lexer",
      "org.xwiki.query.jpql.node.ASingleOrderbyList",
      "org.xwiki.query.jpql.node.ACollectionMemberDeclaration",
      "org.xwiki.query.jpql.node.AGroupbyItem",
      "org.xwiki.query.jpql.node.ASqrtFunctionsReturningNumerics",
      "org.xwiki.query.jpql.node.PTrimCharacter",
      "org.xwiki.query.jpql.parser.TokenIndex",
      "org.xwiki.query.jpql.node.PSelectExpression",
      "org.xwiki.query.jpql.node.AFetchJoin",
      "org.xwiki.query.jpql.node.TNot",
      "org.xwiki.query.jpql.node.ACountCountExpression",
      "org.xwiki.query.jpql.node.AEmptyCollectionComparisonExpression",
      "org.xwiki.query.jpql.node.TAggregateFunc",
      "org.xwiki.query.jpql.node.TLower",
      "org.xwiki.query.xwql.internal.QueryContext$ObjectInfo",
      "org.xwiki.query.jpql.node.AParamInItem",
      "org.xwiki.query.jpql.node.TTrim",
      "org.xwiki.query.jpql.node.PInList"
    );
  } 
  private static void initMocksToAvoidTimeoutsInTheTests() throws ClassNotFoundException { 
    mock(Class.forName("org.xwiki.bridge.DocumentAccessBridge", false, XWQLtoHQLTranslator_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("org.xwiki.query.xwql.internal.QueryContext", false, XWQLtoHQLTranslator_ESTest_scaffolding.class.getClassLoader()));
  }
}
