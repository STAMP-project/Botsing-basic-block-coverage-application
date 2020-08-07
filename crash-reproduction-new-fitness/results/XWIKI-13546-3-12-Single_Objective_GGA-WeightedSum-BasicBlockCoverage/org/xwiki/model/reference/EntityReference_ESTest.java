/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 13:40:34 UTC 2020
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
      EntityType entityType0 = EntityType.DOCUMENT;
      EntityReference entityReference0 = new EntityReference("[iR", entityType0);
      EntityReference entityReference1 = new EntityReference("An Entity Reference name cannot be null or empty", entityType0);
      EntityType entityType1 = EntityType.OBJECT;
      String string0 = "\\{";
      EntityReference entityReference2 = new EntityReference("{", entityType1);
      entityReference0.compareTo(entityReference1);
      EntityType entityType2 = EntityType.BLOCK;
      EntityReference entityReference3 = new EntityReference((String) null, entityType2);
  }
}
