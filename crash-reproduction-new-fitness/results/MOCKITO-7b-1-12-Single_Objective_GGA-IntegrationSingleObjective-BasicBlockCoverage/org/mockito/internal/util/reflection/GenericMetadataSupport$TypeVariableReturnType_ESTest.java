/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 00:02:13 UTC 2020
 */

package org.mockito.internal.util.reflection;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.lang.reflect.Type;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;
import org.mockito.internal.util.reflection.GenericMetadataSupport;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class GenericMetadataSupport$TypeVariableReturnType_ESTest extends GenericMetadataSupport$TypeVariableReturnType_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Type type0 = mock(Type.class, new ViolatedAssumptionAnswer());
      doReturn((String) null).when(type0).toString();
      // Undeclared exception!
      GenericMetadataSupport.inferFrom(type0);
  }
}
