/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 17:13:25 UTC 2020
 */

package org.xwiki.query.internal;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.xwiki.query.internal.CountDocumentFilter;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class CountDocumentFilter_ESTest extends CountDocumentFilter_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      CountDocumentFilter countDocumentFilter0 = new CountDocumentFilter();
      CountDocumentFilter countDocumentFilter1 = new CountDocumentFilter();
      CountDocumentFilter countDocumentFilter2 = new CountDocumentFilter();
      CountDocumentFilter countDocumentFilter3 = new CountDocumentFilter();
      Logger logger0 = mock(Logger.class, new ViolatedAssumptionAnswer());
      Integer integer0 = new Integer((-1187));
      countDocumentFilter3.filterStatement("=j?J5h0% 4RkT-x*", (String) null);
      // Undeclared exception!
      countDocumentFilter3.filterStatement("Iqt_aB& .H~Y8", "hql");
  }
}
