/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:57:34 UTC 2020
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
      BooleanUtils.toBoolean((String) null, (String) null, (String) null);
      BooleanUtils.toBoolean("t,O6");
      String string0 = "[n:j8q#dl3.`z\\`";
      BooleanUtils.toBoolean("yes");
      BooleanUtils.toBoolean("Ou");
      BooleanUtils.toBooleanObject("yes");
      Boolean.getBoolean("Ks7p?~[`cq*");
      boolean boolean0 = true;
      Boolean.logicalXor(false, true);
      String string1 = "off";
      boolean boolean1 = true;
      Boolean.logicalOr(true, true);
      // Undeclared exception!
      BooleanUtils.toBoolean("tru");
  }
}
