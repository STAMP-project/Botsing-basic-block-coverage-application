/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 13:33:53 UTC 2020
 */

package org.xwiki.model.reference;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;
import org.xwiki.model.EntityType;
import org.xwiki.model.reference.WikiReference;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class WikiReference_ESTest extends WikiReference_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      WikiReference wikiReference0 = new WikiReference("Dkv|at8Tv(Z_");
      EntityType entityType0 = EntityType.ATTACHMENT;
      wikiReference0.extractReference(entityType0);
      WikiReference wikiReference1 = new WikiReference("");
  }
}
