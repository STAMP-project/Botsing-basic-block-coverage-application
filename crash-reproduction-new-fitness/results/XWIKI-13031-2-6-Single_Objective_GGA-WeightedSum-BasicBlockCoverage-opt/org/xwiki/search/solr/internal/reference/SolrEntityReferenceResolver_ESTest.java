/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 16:22:06 UTC 2021
 */

package org.xwiki.search.solr.internal.reference;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.solr.common.SolrDocument;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.junit.runner.RunWith;
import org.xwiki.model.EntityType;
import org.xwiki.model.internal.reference.DefaultReferenceEntityReferenceResolver;
import org.xwiki.model.internal.reference.DefaultStringEntityReferenceResolver;
import org.xwiki.model.reference.EntityReferenceProvider;
import org.xwiki.model.reference.EntityReferenceResolver;
import org.xwiki.search.solr.internal.reference.SolrEntityReferenceResolver;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class SolrEntityReferenceResolver_ESTest extends SolrEntityReferenceResolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      EntityReferenceResolver<DefaultReferenceEntityReferenceResolver> entityReferenceResolver0 = (EntityReferenceResolver<DefaultReferenceEntityReferenceResolver>) mock(EntityReferenceResolver.class, new ViolatedAssumptionAnswer());
      SolrDocument solrDocument0 = new SolrDocument();
      solrDocument0.removeFields("wiki");
      Object object0 = new Object();
      Object object1 = new Object();
      SolrEntityReferenceResolver solrEntityReferenceResolver0 = new SolrEntityReferenceResolver();
      DefaultStringEntityReferenceResolver defaultStringEntityReferenceResolver0 = new DefaultStringEntityReferenceResolver();
      solrDocument0.put("wiki", (Object) entityReferenceResolver0);
      EntityReferenceProvider entityReferenceProvider0 = mock(EntityReferenceProvider.class, new ViolatedAssumptionAnswer());
      Injector.inject(defaultStringEntityReferenceResolver0, (Class<?>) DefaultStringEntityReferenceResolver.class, "provider", (Object) entityReferenceProvider0);
      Injector.validateBean(defaultStringEntityReferenceResolver0, (Class<?>) DefaultStringEntityReferenceResolver.class);
      Injector.inject(solrEntityReferenceResolver0, (Class<?>) SolrEntityReferenceResolver.class, "explicitReferenceEntityReferenceResolver", (Object) defaultStringEntityReferenceResolver0);
      Injector.validateBean(solrEntityReferenceResolver0, (Class<?>) SolrEntityReferenceResolver.class);
      EntityType entityType0 = EntityType.BLOCK;
      Object[] objectArray0 = new Object[19];
      objectArray0[0] = object1;
      // Undeclared exception!
      solrEntityReferenceResolver0.resolve(solrDocument0, entityType0, objectArray0);
  }
}
