/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 15:28:08 UTC 2021
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
import org.xwiki.model.internal.reference.RelativeStringEntityReferenceResolver;
import org.xwiki.model.internal.reference.SymbolScheme;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractStringEntityReferenceResolver_ESTest extends AbstractStringEntityReferenceResolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DefaultSymbolScheme defaultSymbolScheme0 = new DefaultSymbolScheme();
      RelativeStringEntityReferenceResolver relativeStringEntityReferenceResolver0 = new RelativeStringEntityReferenceResolver(defaultSymbolScheme0);
      SymbolScheme symbolScheme0 = mock(SymbolScheme.class, new ViolatedAssumptionAnswer());
      Injector.inject(relativeStringEntityReferenceResolver0, (Class<?>) AbstractStringEntityReferenceResolver.class, "symbolScheme", (Object) symbolScheme0);
      Injector.validateBean(relativeStringEntityReferenceResolver0, (Class<?>) RelativeStringEntityReferenceResolver.class);
      defaultSymbolScheme0.initialize();
      ExplicitStringEntityReferenceResolver explicitStringEntityReferenceResolver0 = new ExplicitStringEntityReferenceResolver();
      Injector.inject(explicitStringEntityReferenceResolver0, (Class<?>) AbstractStringEntityReferenceResolver.class, "symbolScheme", (Object) defaultSymbolScheme0);
      Injector.validateBean(explicitStringEntityReferenceResolver0, (Class<?>) ExplicitStringEntityReferenceResolver.class);
      explicitStringEntityReferenceResolver0.initialize();
      EntityType entityType0 = EntityType.SPACE;
      Object[] objectArray0 = new Object[5];
      objectArray0[0] = (Object) symbolScheme0;
      Object object0 = new Object();
      objectArray0[1] = object0;
      objectArray0[2] = (Object) relativeStringEntityReferenceResolver0;
      objectArray0[3] = (Object) "";
      objectArray0[4] = (Object) symbolScheme0;
      // Undeclared exception!
      explicitStringEntityReferenceResolver0.resolve("", entityType0, objectArray0);
  }
}
