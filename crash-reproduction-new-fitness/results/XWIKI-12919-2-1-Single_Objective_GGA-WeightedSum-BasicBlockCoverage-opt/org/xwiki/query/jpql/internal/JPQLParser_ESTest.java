/*
 * This file was automatically generated by EvoSuite
 * Tue Oct 26 05:11:59 UTC 2021
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
      String string0 = "6vwLj8^rJb.sqVB";
      try { 
        jPQLParser1.parse("6vwLj8^rJb.sqVB");
        fail("Expecting exception: ParserException");
      
      } catch(ParserException e) {
         //
         // [1,1] expecting: select
         //
         verifyException("org.xwiki.query.jpql.parser.Parser", e);
      }
  }
}
