/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 20:12:01 UTC 2020
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
      EntityType entityType0 = EntityType.OBJECT;
      EntityReference entityReference0 = new EntityReference(" E>I6RD*5", entityType0);
      EntityReference entityReference1 = entityReference0.extractFirstReference(entityType0);
      entityReference1.getParameter("");
      EntityReference entityReference2 = entityReference1.appendParent(entityReference0);
      Object object0 = new Object();
      entityReference2.equals(object0);
      entityReference1.getType();
      entityReference1.toString();
      // Undeclared exception!
      entityReference2.setName((String) null);
  }
}
