/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 17:15:54 UTC 2020
 */

package org.xwiki.model.internal.reference;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.junit.runner.RunWith;
import org.xwiki.model.EntityType;
import org.xwiki.model.internal.reference.AbstractStringEntityReferenceResolver;
import org.xwiki.model.internal.reference.DefaultSymbolScheme;
import org.xwiki.model.internal.reference.ExplicitStringEntityReferenceResolver;
import org.xwiki.model.internal.reference.SymbolScheme;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractStringEntityReferenceResolver_ESTest extends AbstractStringEntityReferenceResolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      SymbolScheme symbolScheme0 = mock(SymbolScheme.class, new ViolatedAssumptionAnswer());
      Object[] objectArray0 = new Object[5];
      objectArray0[0] = (Object) "m9[mRLOWi";
      objectArray0[1] = objectArray0[0];
      objectArray0[3] = (Object) symbolScheme0;
      objectArray0[4] = (Object) "m9[mRLOWi";
      EntityType entityType0 = EntityType.ATTACHMENT;
      Object[] objectArray1 = new Object[4];
      objectArray1[1] = (Object) "[]";
      objectArray1[3] = objectArray1[2];
      ExplicitStringEntityReferenceResolver explicitStringEntityReferenceResolver0 = new ExplicitStringEntityReferenceResolver();
      DefaultSymbolScheme defaultSymbolScheme0 = new DefaultSymbolScheme();
      defaultSymbolScheme0.initialize();
      Injector.inject(explicitStringEntityReferenceResolver0, (Class<?>) AbstractStringEntityReferenceResolver.class, "symbolScheme", (Object) defaultSymbolScheme0);
      Injector.validateBean(explicitStringEntityReferenceResolver0, (Class<?>) ExplicitStringEntityReferenceResolver.class);
      explicitStringEntityReferenceResolver0.initialize();
      EntityType entityType1 = EntityType.BLOCK;
      explicitStringEntityReferenceResolver0.getTypeSetup(entityType1);
      // Undeclared exception!
      explicitStringEntityReferenceResolver0.resolve("", entityType0, objectArray0);
  }
}
