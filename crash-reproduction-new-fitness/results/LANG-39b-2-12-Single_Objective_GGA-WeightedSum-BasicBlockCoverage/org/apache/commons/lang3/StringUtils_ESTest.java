/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 20:27:53 UTC 2020
 */

package org.apache.commons.lang3;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.nio.CharBuffer;
import java.util.LinkedList;
import org.apache.commons.lang3.StringUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class StringUtils_ESTest extends StringUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      LinkedList<CharBuffer> linkedList0 = new LinkedList<CharBuffer>();
      StringUtils.join((Iterable<?>) linkedList0, "(SPt8z==aqV=VQ8j");
      StringUtils.endsWithIgnoreCase("(SPt8z==aqV=VQ8j", "(SPt8z==aqV=VQ8j");
      StringUtils.removeStart("(SPt8z==aqV=VQ8j", "");
      String[] stringArray0 = new String[6];
      stringArray0[0] = "(SPt8z==aqV=VQ8j";
      StringUtils.isBlank("(SPt8z==aqV=VQ8j");
      // Undeclared exception!
      StringUtils.replaceEach("(SPt8z==aqV=VQ8j", stringArray0, stringArray0);
  }
}
