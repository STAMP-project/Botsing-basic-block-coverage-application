/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 15:54:48 UTC 2020
 */

package org.xwiki.notifications.internal.email;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.function.Consumer;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.junit.runner.RunWith;
import org.xwiki.bridge.DocumentAccessBridge;
import org.xwiki.notifications.email.NotificationEmailRenderer;
import org.xwiki.notifications.internal.email.AbstractMimeMessageIterator;
import org.xwiki.notifications.internal.email.PeriodicMimeMessageIterator;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractMimeMessageIterator_ESTest extends AbstractMimeMessageIterator_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      PeriodicMimeMessageIterator periodicMimeMessageIterator0 = new PeriodicMimeMessageIterator();
      NotificationEmailRenderer notificationEmailRenderer0 = mock(NotificationEmailRenderer.class, new ViolatedAssumptionAnswer());
      Injector.inject(periodicMimeMessageIterator0, (Class<?>) AbstractMimeMessageIterator.class, "defaultNotificationEmailRenderer", (Object) notificationEmailRenderer0);
      DocumentAccessBridge documentAccessBridge0 = mock(DocumentAccessBridge.class, new ViolatedAssumptionAnswer());
      Injector.inject(periodicMimeMessageIterator0, (Class<?>) AbstractMimeMessageIterator.class, "documentAccessBridge", (Object) documentAccessBridge0);
      Consumer<Object> consumer0 = (Consumer<Object>) mock(Consumer.class, new ViolatedAssumptionAnswer());
      periodicMimeMessageIterator0.forEach(consumer0);
      periodicMimeMessageIterator0.hasNext();
      periodicMimeMessageIterator0.hasNext();
      // Undeclared exception!
      periodicMimeMessageIterator0.next();
  }
}
