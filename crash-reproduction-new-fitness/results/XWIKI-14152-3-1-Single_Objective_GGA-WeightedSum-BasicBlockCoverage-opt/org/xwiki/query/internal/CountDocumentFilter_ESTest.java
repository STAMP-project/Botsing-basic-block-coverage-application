/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 15:24:26 UTC 2021
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
import org.xwiki.query.internal.CountDocumentFilter;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class CountDocumentFilter_ESTest extends CountDocumentFilter_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      CountDocumentFilter countDocumentFilter0 = new CountDocumentFilter();
      Logger logger0 = mock(Logger.class, new ViolatedAssumptionAnswer());
      Injector.inject(countDocumentFilter0, (Class<?>) CountDocumentFilter.class, "logger", (Object) logger0);
      Injector.validateBean(countDocumentFilter0, (Class<?>) CountDocumentFilter.class);
      String string0 = "";
      countDocumentFilter0.getOrderByColumns("");
      countDocumentFilter0.filterStatement("", "55/Y=E.v<G");
      CountDocumentFilter countDocumentFilter1 = new CountDocumentFilter();
      Logger logger1 = mock(Logger.class, new ViolatedAssumptionAnswer());
      Injector.inject(countDocumentFilter1, (Class<?>) CountDocumentFilter.class, "logger", (Object) logger1);
      Injector.validateBean(countDocumentFilter1, (Class<?>) CountDocumentFilter.class);
      countDocumentFilter1.filterStatement("r7R1R$0\"6Po}0uXEB", "je~$Utf=&x");
      String string1 = "hql";
      // Undeclared exception!
      countDocumentFilter1.filterStatement("]", "hql");
  }
}
