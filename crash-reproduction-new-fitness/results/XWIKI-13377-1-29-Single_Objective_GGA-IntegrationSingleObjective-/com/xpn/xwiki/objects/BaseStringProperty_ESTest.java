/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 18:56:37 UTC 2020
 */

package com.xpn.xwiki.objects;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.objects.BaseStringProperty;
import java.time.temporal.ChronoUnit;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseStringProperty_ESTest extends BaseStringProperty_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BaseStringProperty baseStringProperty0 = new BaseStringProperty();
      baseStringProperty0.getObject();
      baseStringProperty0.getClassType();
      baseStringProperty0.setValue("com.xpn.xwiki.objects.BaseStringProperty");
      ChronoUnit chronoUnit0 = ChronoUnit.SECONDS;
      // Undeclared exception!
      baseStringProperty0.setValue(chronoUnit0);
  }
}
