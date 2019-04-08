package org.sentilo.agent.federation;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ImportResource;
import org.springframework.scheduling.annotation.EnableScheduling;

@SpringBootApplication
@EnableScheduling
@ImportResource("classpath:spring/federation-context.xml")
public class SentiloConfAgentServer {

  public static void main(final String[] args) throws Exception {
    SpringApplication.run(SentiloConfAgentServer.class, args);
  }
}
