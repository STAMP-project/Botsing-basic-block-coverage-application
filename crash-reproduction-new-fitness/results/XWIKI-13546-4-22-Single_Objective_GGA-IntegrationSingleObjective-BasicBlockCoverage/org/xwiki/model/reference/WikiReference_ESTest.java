/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 18:05:23 UTC 2020
 */

package org.xwiki.model.reference;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;
import org.xwiki.model.EntityType;
import org.xwiki.model.reference.EntityReference;
import org.xwiki.model.reference.WikiReference;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class WikiReference_ESTest extends WikiReference_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      String string0 = "D";
      WikiReference wikiReference0 = new WikiReference("D");
      EntityType entityType0 = wikiReference0.getType();
      EntityReference entityReference0 = new EntityReference((String) null, entityType0);
  }
}
