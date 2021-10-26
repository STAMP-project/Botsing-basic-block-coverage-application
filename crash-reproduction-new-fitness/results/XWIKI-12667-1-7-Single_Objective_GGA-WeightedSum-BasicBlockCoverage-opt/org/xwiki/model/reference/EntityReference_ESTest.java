/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 15:48:30 UTC 2021
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
      String string0 = " 2Z";
      String string1 = "8pwH_T329LXjV//t7E";
      EntityType entityType0 = EntityType.SPACE;
      EntityReference entityReference0 = new EntityReference("pQ1DFr?.}\u0006w", entityType0);
      String string2 = "Provided reference should not be null";
      entityReference0.setName("Provided reference should not be null");
      entityReference0.getParameter("] does not belong to the parents chain of the reference [");
      String string3 = null;
      EntityType entityType1 = EntityType.BLOCK;
      EntityReference entityReference1 = new EntityReference((String) null, entityType1);
  }
}
