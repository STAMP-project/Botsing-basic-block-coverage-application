/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 10:51:36 UTC 2020
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
      JPQLParser jPQLParser2 = new JPQLParser();
      JPQLParser jPQLParser3 = new JPQLParser();
      try { 
        jPQLParser3.parse("");
        fail("Expecting exception: ParserException");
      
      } catch(ParserException e) {
         //
         // [1,1] expecting: select
         //
         verifyException("org.xwiki.query.jpql.parser.Parser", e);
      }
  }
}
