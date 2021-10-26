/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 16:21:47 UTC 2021
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
import org.junit.runner.RunWith;
import org.xwiki.model.EntityType;
import org.xwiki.model.internal.reference.DefaultReferenceEntityReferenceResolver;
import org.xwiki.model.internal.reference.DefaultStringEntityReferenceResolver;
import org.xwiki.model.internal.reference.ExplicitReferenceEntityReferenceResolver;
import org.xwiki.model.reference.EntityReferenceResolver;
import org.xwiki.search.solr.internal.reference.SolrEntityReferenceResolver;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class SolrEntityReferenceResolver_ESTest extends SolrEntityReferenceResolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DefaultStringEntityReferenceResolver defaultStringEntityReferenceResolver0 = new DefaultStringEntityReferenceResolver();
      EntityReferenceResolver<ExplicitReferenceEntityReferenceResolver> entityReferenceResolver0 = (EntityReferenceResolver<ExplicitReferenceEntityReferenceResolver>) mock(EntityReferenceResolver.class, new ViolatedAssumptionAnswer());
      EntityReferenceResolver<DefaultReferenceEntityReferenceResolver> entityReferenceResolver1 = (EntityReferenceResolver<DefaultReferenceEntityReferenceResolver>) mock(EntityReferenceResolver.class, new ViolatedAssumptionAnswer());
      SolrDocument solrDocument0 = new SolrDocument();
      Object[] objectArray0 = new Object[3];
      Object object0 = new Object();
      solrDocument0.put("wiki", object0);
      SolrEntityReferenceResolver solrEntityReferenceResolver0 = new SolrEntityReferenceResolver();
      Object object1 = new Object();
      EntityType entityType0 = EntityType.BLOCK;
      // Undeclared exception!
      solrEntityReferenceResolver0.resolve(solrDocument0, entityType0, objectArray0);
  }
}
