/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:09:12 UTC 2020
 */

package org.apache.commons.lang3;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.lang3.StringUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class StringUtils_ESTest extends StringUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      char[] charArray0 = new char[9];
      charArray0[0] = 'C';
      charArray0[1] = '\\';
      charArray0[2] = 'H';
      charArray0[3] = ']';
      charArray0[4] = '>';
      charArray0[5] = ']';
      charArray0[6] = 'J';
      charArray0[7] = '';
      charArray0[8] = '0';
      StringUtils.indexOfAnyBut("25(:]@", charArray0);
      StringUtils.indexOfAny("25(:]@", charArray0);
      StringUtils.join((Object[]) null, '\\', 4, 0);
      String[] stringArray0 = new String[5];
      stringArray0[0] = "Array element ";
      stringArray0[1] = "Array element ";
      stringArray0[2] = null;
      stringArray0[3] = "Array element ";
      stringArray0[4] = "Array element ";
      // Undeclared exception!
      StringUtils.replaceEach("Array element ", stringArray0, stringArray0);
  }
}
