/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 15:54:35 UTC 2021
 */

package org.xwiki.model.reference;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;
import org.xwiki.model.reference.WikiReference;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class WikiReference_ESTest extends WikiReference_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      String string0 = "Invalid type [";
      WikiReference wikiReference0 = new WikiReference("Invalid type [");
      wikiReference0.getParameter("");
      wikiReference0.size();
      wikiReference0.getParent();
      wikiReference0.getParent();
      WikiReference wikiReference1 = new WikiReference("");
  }
}
