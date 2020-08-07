/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 13:44:56 UTC 2020
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
      EntityType entityType0 = EntityType.OBJECT_PROPERTY;
      String string0 = "jha8dz`6_D,&%AYr=e";
      EntityType entityType1 = EntityType.DOCUMENT;
      EntityReference entityReference0 = new EntityReference("jha8dz`6_D,&%AYr=e", entityType1, (EntityReference) null);
      EntityType entityType2 = EntityType.OBJECT;
      entityReference0.extractReference(entityType2);
      String string1 = "";
      EntityType entityType3 = EntityType.BLOCK;
      EntityReference entityReference1 = new EntityReference("", entityType3);
  }
}
