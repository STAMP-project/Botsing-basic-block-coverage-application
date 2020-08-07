/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 12:16:03 UTC 2020
 */

package com.xpn.xwiki.store.hibernate.query;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.store.hibernate.HibernateSessionFactory;
import com.xpn.xwiki.store.hibernate.query.HqlQueryExecutor;
import java.util.Map;
import javax.inject.Provider;
import org.apache.commons.chain.web.servlet.ServletWebContext;
import org.apache.struts.chain.contexts.WebActionContext;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.hibernate.Query;
import org.hibernate.collection.PersistentMap;
import org.hibernate.engine.SessionImplementor;
import org.hibernate.engine.query.OrdinalParameterDescriptor;
import org.hibernate.engine.query.ParameterMetadata;
import org.hibernate.impl.CollectionFilterImpl;
import org.hibernate.type.CompositeCustomType;
import org.hibernate.usertype.CompositeUserType;
import org.junit.runner.RunWith;
import org.xwiki.component.embed.EmbeddableComponentManager;
import org.xwiki.context.Execution;
import org.xwiki.job.event.status.JobProgressManager;
import org.xwiki.security.authorization.ContextualAuthorizationManager;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class HqlQueryExecutor_ESTest extends HqlQueryExecutor_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ContextualAuthorizationManager contextualAuthorizationManager0 = mock(ContextualAuthorizationManager.class, new ViolatedAssumptionAnswer());
      Execution execution0 = mock(Execution.class, new ViolatedAssumptionAnswer());
      JobProgressManager jobProgressManager0 = mock(JobProgressManager.class, new ViolatedAssumptionAnswer());
      HibernateSessionFactory hibernateSessionFactory0 = mock(HibernateSessionFactory.class, new ViolatedAssumptionAnswer());
      Query query0 = mock(Query.class, new ViolatedAssumptionAnswer());
      EmbeddableComponentManager embeddableComponentManager0 = new EmbeddableComponentManager("");
      HqlQueryExecutor.isSafeSelect("");
      HqlQueryExecutor hqlQueryExecutor0 = new HqlQueryExecutor();
      ContextualAuthorizationManager contextualAuthorizationManager1 = mock(ContextualAuthorizationManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "authorization", (Object) contextualAuthorizationManager1);
      Provider<Integer> provider0 = (Provider<Integer>) mock(Provider.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "componentManagerProvider", (Object) provider0);
      Execution execution1 = mock(Execution.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "execution", (Object) execution1);
      JobProgressManager jobProgressManager1 = mock(JobProgressManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "progress", (Object) jobProgressManager1);
      HibernateSessionFactory hibernateSessionFactory1 = mock(HibernateSessionFactory.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "sessionFactory", (Object) hibernateSessionFactory1);
      Injector.validateBean(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class);
      SessionImplementor sessionImplementor0 = mock(SessionImplementor.class, new ViolatedAssumptionAnswer());
      OrdinalParameterDescriptor[] ordinalParameterDescriptorArray0 = new OrdinalParameterDescriptor[1];
      CompositeUserType compositeUserType0 = mock(CompositeUserType.class, new ViolatedAssumptionAnswer());
      CompositeCustomType compositeCustomType0 = new CompositeCustomType(compositeUserType0);
      OrdinalParameterDescriptor ordinalParameterDescriptor0 = new OrdinalParameterDescriptor((-15), compositeCustomType0, (-15));
      ordinalParameterDescriptorArray0[0] = ordinalParameterDescriptor0;
      ServletWebContext servletWebContext0 = new ServletWebContext();
      WebActionContext webActionContext0 = new WebActionContext(servletWebContext0);
      ParameterMetadata parameterMetadata0 = new ParameterMetadata(ordinalParameterDescriptorArray0, webActionContext0);
      CollectionFilterImpl collectionFilterImpl0 = new CollectionFilterImpl("", execution0, sessionImplementor0, parameterMetadata0);
      SessionImplementor sessionImplementor1 = mock(SessionImplementor.class, new ViolatedAssumptionAnswer());
      PersistentMap persistentMap0 = new PersistentMap(sessionImplementor1, servletWebContext0);
      Query query1 = collectionFilterImpl0.setProperties((Map) persistentMap0);
      // Undeclared exception!
      hqlQueryExecutor0.setNamedParameter(query1, "", collectionFilterImpl0);
  }
}
