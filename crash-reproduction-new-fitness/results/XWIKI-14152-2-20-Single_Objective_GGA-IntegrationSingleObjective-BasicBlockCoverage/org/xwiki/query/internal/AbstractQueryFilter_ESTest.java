/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 17:09:46 UTC 2020
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
public class AbstractQueryFilter_ESTest extends AbstractQueryFilter_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      CountDocumentFilter countDocumentFilter0 = new CountDocumentFilter();
      String string0 = "586.rSKNkEC+'y";
      countDocumentFilter0.filterStatement("586.rSKNkEC+'y", " asc");
      LinkedList<Object> linkedList0 = new LinkedList<Object>();
      countDocumentFilter0.filterResults(linkedList0);
      String string1 = "";
      countDocumentFilter0.getOrderByColumns("");
      String string2 = "X9&8ZvUD%dnJ$NfGRjB";
      // Undeclared exception!
      countDocumentFilter0.getSelectColumns("X9&8ZvUD%dnJ$NfGRjB");
  }
}
