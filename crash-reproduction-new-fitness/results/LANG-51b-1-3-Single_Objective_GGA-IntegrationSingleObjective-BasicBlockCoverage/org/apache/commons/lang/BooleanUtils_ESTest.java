/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:33:30 UTC 2020
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
      Boolean boolean0 = Boolean.valueOf(" is not a valid number.");
      BooleanUtils.isNotFalse(boolean0);
      BooleanUtils.toString(boolean0, (String) null, (String) null, " is not a valid number.");
      BooleanUtils.toBoolean((String) null);
      BooleanUtils.toBoolean("--");
      BooleanUtils.toBoolean("yes");
      BooleanUtils.toBoolean("~es");
      BooleanUtils.toBoolean("rn[+ wvWt*`N@Y");
      // Undeclared exception!
      BooleanUtils.toBoolean("tru");
  }
}
