/*
 * Sentilo
 * 
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
 * 
 * This program is licensed and may be used, modified and redistributed under the terms of the
 * European Public License (EUPL), either version 1.1 or (at your option) any later version as soon
 * as they are approved by the European Commission.
 * 
 * Alternatively, you may redistribute and/or modify this program under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation; either version 3 of the
 * License, or (at your option) any later version.
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied.
 * 
 * See the licenses for the specific language governing permissions, limitations and more details.
 * 
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along with this program;
 * if not, you may find them at:
 * 
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl http://www.gnu.org/licenses/ and
 * https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.common.parser;

import java.io.ByteArrayOutputStream;
import java.lang.reflect.Constructor;

import org.codehaus.jackson.JsonEncoding;
import org.codehaus.jackson.JsonGenerator;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.type.JavaType;
import org.sentilo.common.exception.MessageNotReadableException;
import org.sentilo.common.exception.MessageNotWritableException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.StringUtils;

public class BaseJsonMessageConverter {

  protected static final JsonEncoding DEFAULT_ENCODING = JsonEncoding.UTF8;

  private final Logger logger = LoggerFactory.getLogger(BaseJsonMessageConverter.class);
  private final ObjectMapper objectMapper = new ObjectMapper();

  protected ByteArrayOutputStream writeInternal(final Object o) throws MessageNotWritableException {
    final ByteArrayOutputStream out = new ByteArrayOutputStream();
    try {
      final JsonGenerator jsonGenerator = objectMapper.getJsonFactory().createJsonGenerator(out, DEFAULT_ENCODING);
      objectMapper.writeValue(jsonGenerator, o);
      return out;
    } catch (final Exception e) {
      throw new MessageNotWritableException(e);
    }
  }

  protected String writeInternalAndReturnString(final Object o) throws MessageNotWritableException {
    final ByteArrayOutputStream out = writeInternal(o);
    return out.toString();
  }

  /**
   * Serializa el contenido de <code>json</code> en un objeto de tipo <code>clazz</code>. Si json es
   * null o no tiene contenido, retorna el resultado de invocar al constructor vacio de
   * <code>clazz</code>
   * 
   * @param clazz
   * @param json
   * @return
   * @throws MessageNotReadableException
   */
  protected Object readInternal(final Class<?> clazz, final String json) throws MessageNotReadableException {
    final JavaType javaType = getJavaType(clazz);
    try {
      if (StringUtils.hasText(json)) {
        return objectMapper.readValue(json, javaType);
      } else {
        return buildDefaultInstance(clazz);
      }
    } catch (final Exception e) {
      throw new MessageNotReadableException(e);
    }
  }

  @SuppressWarnings("rawtypes")
  protected Object buildDefaultInstance(final Class<?> clazz) {
    try {
      final Class[] empty = {};
      final Constructor<?> defaultConstructor = clazz.getConstructor(empty);
      return defaultConstructor.newInstance((Object[]) null);
    } catch (final Throwable e) {
      logger.warn("Error calling default constructor of class {}: {}", clazz.getName(), e);
      return null;
    }
  }

  protected JavaType getJavaType(final Class<?> clazz) {
    return objectMapper.getTypeFactory().constructType(clazz);
  }

  protected Logger getLogger() {
    return logger;
  }
}
