/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 13:47:18 UTC 2020
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

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class EntityReference_ESTest extends EntityReference_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      String string0 = "0{_?6";
      EntityType entityType0 = EntityType.WIKI;
      String string1 = "!a";
      EntityReference entityReference0 = new EntityReference("!a", entityType0);
      // Undeclared exception!
      entityReference0.setName("");
  }
}
