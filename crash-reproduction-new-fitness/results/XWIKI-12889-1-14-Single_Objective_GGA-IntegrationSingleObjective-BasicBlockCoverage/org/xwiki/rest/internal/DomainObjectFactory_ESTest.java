/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 06:10:32 UTC 2020
 */

package org.xwiki.rest.internal;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import ch.qos.logback.classic.Logger;
import java.net.URI;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.hibernate.loader.custom.sql.SQLCustomQuery;
import org.junit.runner.RunWith;
import org.xwiki.component.manager.ComponentManager;
import org.xwiki.job.DefaultJobStatus;
import org.xwiki.job.DefaultRequest;
import org.xwiki.job.event.status.JobStatus;
import org.xwiki.logging.logback.internal.DefaultLoggerManager;
import org.xwiki.observation.ObservationManager;
import org.xwiki.observation.internal.DefaultObservationManager;
import org.xwiki.rest.internal.DomainObjectFactory;
import org.xwiki.rest.model.jaxb.ObjectFactory;
import ucar.nc2.util.net.URLStreamHandlerFactory;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DomainObjectFactory_ESTest extends DomainObjectFactory_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DomainObjectFactory domainObjectFactory0 = new DomainObjectFactory();
      ObjectFactory objectFactory0 = mock(ObjectFactory.class, new ViolatedAssumptionAnswer());
      JobStatus jobStatus0 = mock(JobStatus.class, new ViolatedAssumptionAnswer());
      ObjectFactory objectFactory1 = new ObjectFactory();
      ObjectFactory objectFactory2 = new ObjectFactory();
      DefaultRequest defaultRequest0 = new DefaultRequest();
      DefaultObservationManager defaultObservationManager0 = new DefaultObservationManager();
      ComponentManager componentManager0 = mock(ComponentManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(defaultObservationManager0, (Class<?>) DefaultObservationManager.class, "componentManager", (Object) componentManager0);
      Logger logger0 = (Logger)URLStreamHandlerFactory.log;
      Injector.inject(defaultObservationManager0, (Class<?>) DefaultObservationManager.class, "logger", (Object) logger0);
      Injector.validateBean(defaultObservationManager0, (Class<?>) DefaultObservationManager.class);
      DefaultLoggerManager defaultLoggerManager0 = new DefaultLoggerManager();
      Logger logger1 = (Logger)SQLCustomQuery.log;
      Injector.inject(defaultLoggerManager0, (Class<?>) DefaultLoggerManager.class, "logger", (Object) logger1);
      ObservationManager observationManager0 = mock(ObservationManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(defaultLoggerManager0, (Class<?>) DefaultLoggerManager.class, "observation", (Object) observationManager0);
      Injector.validateBean(defaultLoggerManager0, (Class<?>) DefaultLoggerManager.class);
      DefaultJobStatus<DefaultRequest> defaultJobStatus0 = new DefaultJobStatus<DefaultRequest>(defaultRequest0, jobStatus0, defaultObservationManager0, defaultLoggerManager0);
      // Undeclared exception!
      DomainObjectFactory.createJobStatus(objectFactory2, (URI) null, defaultJobStatus0);
  }
}
