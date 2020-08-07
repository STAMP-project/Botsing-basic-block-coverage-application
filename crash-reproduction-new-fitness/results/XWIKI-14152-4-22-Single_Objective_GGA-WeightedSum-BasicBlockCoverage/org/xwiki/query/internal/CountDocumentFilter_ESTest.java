/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 17:10:33 UTC 2020
 */

package org.xwiki.query.internal;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.LinkedList;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;
import org.xwiki.query.internal.CountDocumentFilter;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class CountDocumentFilter_ESTest extends CountDocumentFilter_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      CountDocumentFilter countDocumentFilter0 = new CountDocumentFilter();
      LinkedList<Object> linkedList0 = new LinkedList<Object>();
      countDocumentFilter0.filterResults(linkedList0);
      countDocumentFilter0.filterStatement("h{78w+/", "kX'5/Xs");
      countDocumentFilter0.filterStatement("h{78w+/", "h{78w+/");
      // Undeclared exception!
      countDocumentFilter0.filterStatement("Finished rehash aware operation for %s", "hql");
  }
}
