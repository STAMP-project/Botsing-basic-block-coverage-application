/*
 * This file was automatically generated by EvoSuite
 * Tue May 19 07:55:18 UTC 2020
 */

package org.mockito.internal.creation.instance;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;
import org.mockito.internal.creation.instance.ConstructorInstantiator;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class ConstructorInstantiator_ESTest extends ConstructorInstantiator_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Object object0 = new Object();
      ConstructorInstantiator constructorInstantiator0 = new ConstructorInstantiator(object0);
      Class<Integer> class0 = Integer.class;
      // Undeclared exception!
      constructorInstantiator0.newInstance((Class<?>) class0);
  }
}
