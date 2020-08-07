/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 18:06:22 UTC 2020
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
      WikiReference wikiReference0 = new WikiReference("/*V{F`*P{3i?-wyquH5");
      EntityType entityType0 = EntityType.OBJECT_PROPERTY;
      wikiReference0.extractReference(entityType0);
      wikiReference0.appendParent((EntityReference) null);
      WikiReference wikiReference1 = new WikiReference("(t;+,hRtj");
      WikiReference wikiReference2 = new WikiReference("");
  }
}
