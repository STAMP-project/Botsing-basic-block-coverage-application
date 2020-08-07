/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 15:58:12 UTC 2020
 */

package com.xpn.xwiki.store.hibernate.query;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.store.hibernate.HibernateSessionFactory;
import com.xpn.xwiki.store.hibernate.query.HqlQueryExecutor;
import java.util.Map;
import java.util.Queue;
import javax.inject.Provider;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.hibernate.Query;
import org.hibernate.engine.SessionImplementor;
import org.hibernate.engine.query.OrdinalParameterDescriptor;
import org.hibernate.engine.query.ParameterMetadata;
import org.hibernate.impl.CollectionFilterImpl;
import org.hibernate.transform.DistinctResultTransformer;
import org.hibernate.type.UUIDCharType;
import org.junit.runner.RunWith;
import org.slf4j.event.SubstituteLoggingEvent;
import org.slf4j.helpers.SubstituteLogger;
import org.xwiki.component.internal.ContextRootComponentManager;
import org.xwiki.context.Execution;
import org.xwiki.job.event.status.JobProgressManager;
import org.xwiki.query.internal.CountDocumentFilter;
import org.xwiki.security.authorization.ContextualAuthorizationManager;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class HqlQueryExecutor_ESTest extends HqlQueryExecutor_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      HqlQueryExecutor hqlQueryExecutor0 = new HqlQueryExecutor();
      ContextualAuthorizationManager contextualAuthorizationManager0 = mock(ContextualAuthorizationManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "authorization", (Object) contextualAuthorizationManager0);
      Provider<ContextRootComponentManager> provider0 = (Provider<ContextRootComponentManager>) mock(Provider.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "componentManagerProvider", (Object) provider0);
      Execution execution0 = mock(Execution.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "execution", (Object) execution0);
      JobProgressManager jobProgressManager0 = mock(JobProgressManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "progress", (Object) jobProgressManager0);
      HibernateSessionFactory hibernateSessionFactory0 = mock(HibernateSessionFactory.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "sessionFactory", (Object) hibernateSessionFactory0);
      Injector.validateBean(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class);
      Query query0 = mock(Query.class, new ViolatedAssumptionAnswer());
      CountDocumentFilter countDocumentFilter0 = new CountDocumentFilter();
      SubstituteLogger substituteLogger0 = new SubstituteLogger("~~-Z@5!!.t", (Queue<SubstituteLoggingEvent>) null, true);
      Injector.inject(countDocumentFilter0, (Class<?>) CountDocumentFilter.class, "logger", (Object) substituteLogger0);
      Injector.validateBean(countDocumentFilter0, (Class<?>) CountDocumentFilter.class);
      SessionImplementor sessionImplementor0 = mock(SessionImplementor.class, new ViolatedAssumptionAnswer());
      OrdinalParameterDescriptor[] ordinalParameterDescriptorArray0 = new OrdinalParameterDescriptor[3];
      UUIDCharType uUIDCharType0 = new UUIDCharType();
      OrdinalParameterDescriptor ordinalParameterDescriptor0 = new OrdinalParameterDescriptor(129, uUIDCharType0, 1290);
      ordinalParameterDescriptorArray0[0] = ordinalParameterDescriptor0;
      OrdinalParameterDescriptor ordinalParameterDescriptor1 = new OrdinalParameterDescriptor(0, uUIDCharType0, 129);
      ordinalParameterDescriptorArray0[1] = ordinalParameterDescriptor1;
      OrdinalParameterDescriptor ordinalParameterDescriptor2 = new OrdinalParameterDescriptor(1290, uUIDCharType0, 0);
      ordinalParameterDescriptorArray0[2] = ordinalParameterDescriptor2;
      ParameterMetadata parameterMetadata0 = new ParameterMetadata(ordinalParameterDescriptorArray0, (Map) null);
      CollectionFilterImpl collectionFilterImpl0 = new CollectionFilterImpl("~~-Z@5!!.t", countDocumentFilter0, sessionImplementor0, parameterMetadata0);
      DistinctResultTransformer distinctResultTransformer0 = DistinctResultTransformer.INSTANCE;
      Query query1 = collectionFilterImpl0.setResultTransformer(distinctResultTransformer0);
      // Undeclared exception!
      hqlQueryExecutor0.setNamedParameter(query1, "~~-Z@5!!.t", uUIDCharType0);
  }
}
