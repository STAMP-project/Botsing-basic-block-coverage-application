/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 18:34:49 UTC 2020
 */

package com.xpn.xwiki.objects;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.objects.BaseStringProperty;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;
import org.xwiki.model.reference.LocalDocumentReference;
import org.xwiki.model.reference.SpaceReference;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseStringProperty_ESTest extends BaseStringProperty_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BaseStringProperty baseStringProperty0 = new BaseStringProperty();
      String[] stringArray0 = new String[1];
      stringArray0[0] = "YJf+Q,@u|i7>X;2Xh";
      SpaceReference spaceReference0 = new SpaceReference("YJf+Q,@u|i7>X;2Xh", stringArray0);
      LocalDocumentReference localDocumentReference0 = new LocalDocumentReference(spaceReference0);
      // Undeclared exception!
      baseStringProperty0.setValue(localDocumentReference0);
  }
}
