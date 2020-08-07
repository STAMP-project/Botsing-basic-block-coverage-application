/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 17:10:58 UTC 2020
 */

package org.xwiki.query.internal;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.xwiki.query.internal.UniqueDocumentFilter;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractQueryFilter_ESTest extends AbstractQueryFilter_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      UniqueDocumentFilter uniqueDocumentFilter0 = new UniqueDocumentFilter();
      Logger logger0 = mock(Logger.class, new ViolatedAssumptionAnswer());
      Injector.inject(uniqueDocumentFilter0, (Class<?>) UniqueDocumentFilter.class, "logger", (Object) logger0);
      Injector.validateBean(uniqueDocumentFilter0, (Class<?>) UniqueDocumentFilter.class);
      uniqueDocumentFilter0.filterStatement("7{-s4y]t-)V;H/?);E", "F'M(GpJ>cj_r");
      // Undeclared exception!
      uniqueDocumentFilter0.getSelectColumns("F'M(GpJ>cj_r");
  }
}
