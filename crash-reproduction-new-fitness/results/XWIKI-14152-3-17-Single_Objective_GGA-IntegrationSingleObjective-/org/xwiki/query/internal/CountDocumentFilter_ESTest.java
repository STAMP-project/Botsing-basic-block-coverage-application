/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 17:08:21 UTC 2020
 */

package org.xwiki.query.internal;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.LinkedList;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.xwiki.query.internal.AbstractQueryFilter;
import org.xwiki.query.internal.CountDocumentFilter;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class CountDocumentFilter_ESTest extends CountDocumentFilter_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      CountDocumentFilter countDocumentFilter0 = new CountDocumentFilter();
      CountDocumentFilter countDocumentFilter1 = new CountDocumentFilter();
      LinkedList<Integer> linkedList0 = new LinkedList<Integer>();
      countDocumentFilter1.filterResults(linkedList0);
      CountDocumentFilter countDocumentFilter2 = new CountDocumentFilter();
      Logger logger0 = mock(Logger.class, new ViolatedAssumptionAnswer());
      linkedList0.toArray();
      Injector.inject(countDocumentFilter2, (Class<?>) CountDocumentFilter.class, "logger", (Object) logger0);
      Injector.validateBean(countDocumentFilter2, (Class<?>) CountDocumentFilter.class);
      LinkedList<Integer> linkedList1 = new LinkedList<Integer>();
      countDocumentFilter1.filterResults(linkedList0);
      countDocumentFilter0.filterResults(linkedList1);
      String string0 = AbstractQueryFilter.FULLNAME_COLUMN;
      countDocumentFilter1.filterStatement("+*|~}d)P`J", "+*|~}d)P`J");
      // Undeclared exception!
      countDocumentFilter1.filterStatement("doc.fullName", "hql");
  }
}
