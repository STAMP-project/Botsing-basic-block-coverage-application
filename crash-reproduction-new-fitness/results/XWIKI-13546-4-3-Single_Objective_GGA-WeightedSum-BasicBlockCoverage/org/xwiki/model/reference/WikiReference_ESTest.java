/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 13:31:50 UTC 2020
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
      String string0 = "startimage:";
      WikiReference wikiReference0 = new WikiReference("startimage:");
      WikiReference wikiReference1 = new WikiReference("startimage:");
      EntityReference entityReference0 = wikiReference1.getRoot();
      EntityType entityType0 = EntityType.WIKI;
      EntityType entityType1 = EntityType.SPACE;
      wikiReference1.equals((EntityReference) wikiReference0, entityType0, entityType1);
      wikiReference0.equalsNonRecursive(entityReference0);
      WikiReference wikiReference2 = new WikiReference(wikiReference1);
      wikiReference0.compareTo(entityReference0);
      WikiReference wikiReference3 = new WikiReference("");
  }
}
