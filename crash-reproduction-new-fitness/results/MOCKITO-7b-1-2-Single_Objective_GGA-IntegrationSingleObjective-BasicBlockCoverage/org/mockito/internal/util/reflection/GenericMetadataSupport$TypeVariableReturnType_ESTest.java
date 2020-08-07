/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 23:56:10 UTC 2020
 */

package org.mockito.internal.util.reflection;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;
import org.mockito.internal.util.reflection.GenericMetadataSupport;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class GenericMetadataSupport$TypeVariableReturnType_ESTest extends GenericMetadataSupport$TypeVariableReturnType_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      GenericMetadataSupport genericMetadataSupport0 = mock(GenericMetadataSupport.class, new ViolatedAssumptionAnswer());
      TypeVariable<Class<GenericMetadataSupport.WildCardBoundedType>>[] typeVariableArray0 = null;
      Type type0 = mock(Type.class, new ViolatedAssumptionAnswer());
      doReturn((String) null).when(type0).toString();
      // Undeclared exception!
      GenericMetadataSupport.inferFrom(type0);
  }
}
