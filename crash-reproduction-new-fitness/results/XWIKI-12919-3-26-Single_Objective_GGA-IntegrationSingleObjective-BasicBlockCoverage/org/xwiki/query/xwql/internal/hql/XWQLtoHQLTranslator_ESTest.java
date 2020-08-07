/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 15:17:51 UTC 2020
 */

package org.xwiki.query.xwql.internal.hql;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.junit.runner.RunWith;
import org.xwiki.bridge.DocumentAccessBridge;
import org.xwiki.query.jpql.parser.ParserException;
import org.xwiki.query.xwql.internal.hql.XWQLtoHQLTranslator;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class XWQLtoHQLTranslator_ESTest extends XWQLtoHQLTranslator_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      XWQLtoHQLTranslator xWQLtoHQLTranslator0 = new XWQLtoHQLTranslator();
      DocumentAccessBridge documentAccessBridge0 = mock(DocumentAccessBridge.class, new ViolatedAssumptionAnswer());
      doReturn((String) null).when(documentAccessBridge0).toString();
      Injector.inject(xWQLtoHQLTranslator0, (Class<?>) XWQLtoHQLTranslator.class, "documentAccessBridge", (Object) documentAccessBridge0);
      Injector.validateBean(xWQLtoHQLTranslator0, (Class<?>) XWQLtoHQLTranslator.class);
      XWQLtoHQLTranslator xWQLtoHQLTranslator1 = new XWQLtoHQLTranslator();
      DocumentAccessBridge documentAccessBridge1 = mock(DocumentAccessBridge.class, new ViolatedAssumptionAnswer());
      Injector.inject(xWQLtoHQLTranslator1, (Class<?>) XWQLtoHQLTranslator.class, "documentAccessBridge", (Object) documentAccessBridge1);
      Injector.validateBean(xWQLtoHQLTranslator1, (Class<?>) XWQLtoHQLTranslator.class);
      xWQLtoHQLTranslator0.getDocumentAccessBridge();
      try { 
        xWQLtoHQLTranslator1.translate("where");
        fail("Expecting exception: ParserException");
      
      } catch(ParserException e) {
         //
         // [1,47] expecting: '(', abs, concat, count, length, locate, lower, mod, not, size, sqrt, substring, trim, upper, boolean literal, integer literal, float literal, string literal, input parameter, aggregate func, add sub, functions returning datetime, id
         //
         verifyException("org.xwiki.query.jpql.parser.Parser", e);
      }
  }
}
