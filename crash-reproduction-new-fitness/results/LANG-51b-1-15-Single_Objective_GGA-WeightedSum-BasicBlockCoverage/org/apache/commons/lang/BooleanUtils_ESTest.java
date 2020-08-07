/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 20:58:06 UTC 2020
 */

package org.apache.commons.lang;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.lang.BooleanUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BooleanUtils_ESTest extends BooleanUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Boolean boolean0 = Boolean.FALSE;
      BooleanUtils.isNotTrue((Boolean) null);
      BooleanUtils.isNotFalse((Boolean) null);
      BooleanUtils.toBoolean("tncD");
      BooleanUtils.toBoolean("");
      String string0 = "ej";
      String string1 = "tnc";
      BooleanUtils.toBoolean("Y0x");
      // Undeclared exception!
      BooleanUtils.toBoolean("tru");
  }
}
