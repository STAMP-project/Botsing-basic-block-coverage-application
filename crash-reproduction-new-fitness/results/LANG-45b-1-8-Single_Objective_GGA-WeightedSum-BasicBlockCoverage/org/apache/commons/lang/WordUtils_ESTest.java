/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 20:55:15 UTC 2020
 */

package org.apache.commons.lang;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.lang.WordUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class WordUtils_ESTest extends WordUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      WordUtils.capitalize("OS/2");
      char[] charArray0 = new char[9];
      charArray0[0] = 'Q';
      charArray0[1] = '[';
      charArray0[2] = 'h';
      charArray0[3] = '9';
      charArray0[4] = 'p';
      charArray0[5] = '\'';
      charArray0[6] = 't';
      charArray0[7] = 'I';
      charArray0[8] = ';';
      WordUtils.initials("OS/2", charArray0);
      WordUtils.abbreviate("OS/2", 0, 0, "O");
      WordUtils.capitalizeFully("O");
      // Undeclared exception!
      WordUtils.abbreviate("OS/2", 808, 808, "");
  }
}
