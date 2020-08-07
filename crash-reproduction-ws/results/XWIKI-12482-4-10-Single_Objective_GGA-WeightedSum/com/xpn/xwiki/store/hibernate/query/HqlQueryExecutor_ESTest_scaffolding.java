/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Mon Mar 30 22:12:32 UTC 2020
 */

package com.xpn.xwiki.store.hibernate.query;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

@EvoSuiteClassExclude
public class HqlQueryExecutor_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "com.xpn.xwiki.store.hibernate.query.HqlQueryExecutor"; 
    org.evosuite.runtime.GuiSupport.initialize(); 
    org.evosuite.runtime.RuntimeSettings.maxNumberOfIterationsPerLoop = 10000; 
    org.evosuite.runtime.RuntimeSettings.mockSystemIn = true; 
    org.evosuite.runtime.Runtime.getInstance().resetRuntime(); 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(HqlQueryExecutor_ESTest_scaffolding.class.getClassLoader() ,
      "com.xpn.xwiki.XWikiException",
      "org.xwiki.component.phase.Initializable",
      "net.sf.jsqlparser.statement.create.table.NamedConstraint",
      "org.xwiki.query.Query",
      "net.sf.jsqlparser.expression.operators.arithmetic.Multiplication",
      "net.sf.jsqlparser.parser.TokenMgrError",
      "net.sf.jsqlparser.parser.Token",
      "net.sf.jsqlparser.parser.CCJSqlParser$LookaheadSuccess",
      "net.sf.jsqlparser.parser.ParseException",
      "com.xpn.xwiki.store.XWikiStoreInterface",
      "org.xwiki.component.phase.InitializationException",
      "net.sf.jsqlparser.expression.operators.arithmetic.BitwiseOr",
      "org.apache.commons.lang3.StringUtils",
      "net.sf.jsqlparser.statement.select.SelectBody",
      "com.xpn.xwiki.store.hibernate.query.HqlQueryExecutor",
      "net.sf.jsqlparser.expression.operators.arithmetic.Addition",
      "org.hibernate.Query",
      "net.sf.jsqlparser.expression.operators.relational.GreaterThanEquals",
      "net.sf.jsqlparser.parser.SimpleCharStream",
      "net.sf.jsqlparser.expression.operators.relational.SupportsOldOracleJoinSyntax",
      "net.sf.jsqlparser.statement.select.PivotXml",
      "net.sf.jsqlparser.expression.operators.arithmetic.Modulo",
      "net.sf.jsqlparser.parser.CCJSqlParser",
      "net.sf.jsqlparser.statement.select.SelectItem",
      "net.sf.jsqlparser.expression.operators.relational.RegExpMySQLOperator",
      "net.sf.jsqlparser.parser.CCJSqlParserUtil",
      "net.sf.jsqlparser.schema.MultiPartName",
      "com.xpn.xwiki.store.XWikiHibernateStore",
      "net.sf.jsqlparser.statement.select.FromItem",
      "net.sf.jsqlparser.expression.operators.relational.GreaterThan",
      "net.sf.jsqlparser.statement.select.Pivot",
      "net.sf.jsqlparser.expression.Expression",
      "net.sf.jsqlparser.expression.operators.relational.MinorThanEquals",
      "net.sf.jsqlparser.expression.operators.arithmetic.Subtraction",
      "net.sf.jsqlparser.statement.select.Select",
      "net.sf.jsqlparser.expression.operators.relational.Matches",
      "net.sf.jsqlparser.expression.operators.relational.ItemsList",
      "net.sf.jsqlparser.statement.create.table.Index",
      "net.sf.jsqlparser.expression.BinaryExpression",
      "net.sf.jsqlparser.parser.CCJSqlParser$JJCalls",
      "org.xwiki.query.QueryException",
      "org.hibernate.Session",
      "org.apache.commons.lang3.ArrayUtils",
      "net.sf.jsqlparser.parser.CCJSqlParserConstants",
      "net.sf.jsqlparser.expression.operators.arithmetic.Division",
      "net.sf.jsqlparser.statement.select.PlainSelect",
      "net.sf.jsqlparser.JSQLParserException",
      "net.sf.jsqlparser.expression.operators.relational.MinorThan",
      "org.apache.commons.lang3.CharSequenceUtils",
      "net.sf.jsqlparser.expression.Function",
      "net.sf.jsqlparser.schema.Column",
      "net.sf.jsqlparser.expression.operators.relational.ExpressionList",
      "com.xpn.xwiki.store.XWikiHibernateBaseStore$HibernateCallback",
      "org.xwiki.query.QueryExecutor",
      "net.sf.jsqlparser.expression.operators.relational.EqualsTo",
      "net.sf.jsqlparser.expression.operators.relational.OldOracleJoinBinaryExpression",
      "com.xpn.xwiki.internal.store.hibernate.query.HqlQueryUtils",
      "net.sf.jsqlparser.expression.operators.arithmetic.BitwiseAnd",
      "net.sf.jsqlparser.expression.operators.relational.RegExpMatchOperator",
      "net.sf.jsqlparser.statement.select.SelectExpressionItem",
      "com.xpn.xwiki.XWikiContext",
      "net.sf.jsqlparser.parser.CCJSqlParserTokenManager",
      "com.xpn.xwiki.store.XWikiHibernateBaseStore",
      "net.sf.jsqlparser.statement.Statement",
      "net.sf.jsqlparser.expression.operators.relational.NotEqualsTo"
    );
  } 
}
