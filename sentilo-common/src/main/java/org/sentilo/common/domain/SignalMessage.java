/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de
 * Barcelona. Modified by Opentrends adding support for multitenant deployments and SaaS. 
 * Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the terms  of the 
 * European Public License (EUPL), either version 1.1 or (at your option) any later version as soon 
 * as they are approved by the European Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms of the GNU Lesser 
 * General Public License as published by the Free Software Foundation; either  version 3 of the 
 * License, or (at your option) any later version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed under the License 
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR  CONDITIONS OF ANY KIND, either express 
 * or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along with this program; 
 * if not, you may find them at: 
 *   
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl http://www.gnu.org/licenses/   and 
 * https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.common.domain;

import org.sentilo.common.enums.SignalType;

public class SignalMessage {

  private long time;
  private SignalType signal;
  private String sender;

  public SignalMessage() {
    super();
  }

  public SignalMessage(final long time, final SignalType signal, final String sender) {
    this();
    this.time = time;
    this.signal = signal;
    this.sender = sender;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("\n--- Signal --- ");
    sb.append("\n\t signal:" + signal.name());
    sb.append("\n\t time:" + time);
    sb.append("\n\t sender:" + sender);
    sb.append("\n");
    return sb.toString();
  }

  public long getTime() {
    return time;
  }

  public void setTime(final long time) {
    this.time = time;
  }

  public SignalType getSignal() {
    return signal;
  }

  public void setSignal(final SignalType signal) {
    this.signal = signal;
  }

  public String getSender() {
    return sender;
  }

  public void setSender(final String sender) {
    this.sender = sender;
  }

}
