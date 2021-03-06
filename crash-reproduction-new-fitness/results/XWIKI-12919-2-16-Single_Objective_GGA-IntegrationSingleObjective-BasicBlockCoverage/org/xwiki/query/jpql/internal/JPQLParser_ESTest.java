/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 15:13:53 UTC 2020
 */

package org.xwiki.query.jpql.internal;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;
import org.xwiki.query.jpql.internal.JPQLParser;
import org.xwiki.query.jpql.parser.ParserException;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class JPQLParser_ESTest extends JPQLParser_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      JPQLParser jPQLParser0 = new JPQLParser();
      JPQLParser jPQLParser1 = new JPQLParser();
      String string0 = " ";
      try { 
        jPQLParser0.parse(" ");
        fail("Expecting exception: ParserException");
      
      } catch(ParserException e) {
         //
         // [1,2] expecting: select
         //
         verifyException("org.xwiki.query.jpql.parser.Parser", e);
      }
  }
}
