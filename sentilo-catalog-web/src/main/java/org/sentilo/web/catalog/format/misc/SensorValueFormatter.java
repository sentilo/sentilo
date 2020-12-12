/*
 * Sentilo
 *
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de
 * Barcelona. Modified by Opentrends adding support for multitenant deployments and SaaS.
 * Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 *
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
package org.sentilo.web.catalog.format.misc;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.apache.commons.validator.routines.UrlValidator;
import org.sentilo.common.config.SentiloArtifactConfigService;
import org.sentilo.platform.client.core.domain.Observation;
import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.Sensor.DataType;
import org.sentilo.web.catalog.service.ApplicationService;
import org.sentilo.web.catalog.utils.Constants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import com.amazonaws.AmazonServiceException;
import com.amazonaws.HttpMethod;
import com.amazonaws.SdkClientException;
import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.AWSStaticCredentialsProvider;
import com.amazonaws.auth.BasicAWSCredentials;
import com.amazonaws.client.builder.AwsClientBuilder.EndpointConfiguration;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3ClientBuilder;
import com.amazonaws.services.s3.model.GeneratePresignedUrlRequest;

@Component
public class SensorValueFormatter {

  private final static int URL_BUCKET_POS = 1;
  private final static int URL_FILENAME_POS = 2;
  private final static int S3_PATH_TOKENS = 3;

  private static Logger LOGGER = LoggerFactory.getLogger(SensorValueFormatter.class);

  @Autowired
  private SentiloArtifactConfigService configService;

  @Autowired
  private ApplicationService applicationService;

  private AWSCredentials awsCredentials;

  /**
   * Depending on sensor data type, value is formatted to be displayed in the view
   *
   * @param sensor
   * @param observation
   * @return
   */
  public String formatValue(final Sensor sensor, final Observation observation) {
    String formattedValue = observation.getValue();

    if (StringUtils.hasText(observation.getValue()) && isLinkDataType(sensor.getDataType())) {
      // Generates a new link based on the source link but with additional parameters associated
      // with the
      // S3 authentication
      formattedValue = formatLink(observation.getValue());
    }

    return formattedValue;
  }

  private boolean isLinkDataType(final DataType sensorDataType) {
    switch (sensorDataType) {
      case LINK:
      case AUDIO_LINK:
      case VIDEO_LINK:
      case FILE_LINK:
      case IMAGE_LINK:
        return true;
      default:
        return false;
    }
  }

  /**
   * If sensor type is of type link and observation value is a link that point to Sentilo S3 server,
   * then value is formatted to sign it with the S3 credentials. Otherwise value remains without
   * modify
   *
   * @param sensor
   * @param observation
   * @return
   */
  private String formatLink(final String url) {

    String formattedLink = url;

    final String s3SigningRegion = configService.getConfigValue("sentilo.s3.signing.region", Constants.S3_SIGNING_REGION);
    final String s3UrlTtl = configService.getConfigValue("sentilo.s3.url.ttl", String.valueOf(Constants.S3_LINK_DEFAULT_TTL));

    final String acceptedS3Enpoints = configService.getConfigValue("sentilo.s3.endpoints");
    if (!StringUtils.hasText(acceptedS3Enpoints)) {
      return formattedLink;
    }

    final List<String> acceptedS3EndpointsList = Arrays.asList(acceptedS3Enpoints.split(","));

    // Get the original request endpoint
    final S3Url s3Object = parseS3ResourceUrl(url);
    if (s3Object.isValid() && acceptedS3EndpointsList.contains(s3Object.getAuthority())) {
      final String endpoint = s3Object.getEndpoint();
      // Is it a Sentilo S3 URL
      try {
        // Create the AWS S3 Client
        final AWSCredentials awsCreds = buildAWSCredentials();
        final AmazonS3 s3Client =
            AmazonS3ClientBuilder.standard().withPathStyleAccessEnabled(true).withCredentials(new AWSStaticCredentialsProvider(awsCreds))
                .withEndpointConfiguration(new EndpointConfiguration(endpoint, s3SigningRegion)).build();

        // Set the link expiration time
        final Date expiration = new Date(System.currentTimeMillis() + Long.valueOf(s3UrlTtl));

        // Generate URL
        final GeneratePresignedUrlRequest generatePresignedUrlRequest =
            new GeneratePresignedUrlRequest(s3Object.getBucket(), s3Object.getFilename()).withMethod(HttpMethod.GET).withExpiration(expiration);

        final URL s3Url = s3Client.generatePresignedUrl(generatePresignedUrlRequest);
        formattedLink = s3Url.toString();

      } catch (final AmazonServiceException e) {
        // The call was transmitted successfully, but Amazon S3 couldn't process it, so it returned
        // an error
        // response.
        LOGGER.error("Error while processing S3 request", e);
      } catch (final SdkClientException e) {
        // Amazon S3 couldn't be contacted for a response, or the client couldn't parse the response
        // from
        // Amazon S3.
        LOGGER.error("Error while connecting to S3 client", e);
      }
    }

    return formattedLink;
  }

  private AWSCredentials buildAWSCredentials() {
    // AWS credentials is equal to Sentilo master application credentials which is authorized to
    // read
    // all files stored into Sentilo S3
    if (awsCredentials == null) {
      final String s3AwsAccessKeyId = configService.getConfigValue(Constants.CATALOG_MASTER_APP_ID);
      final Application catalogApp = applicationService.find(new Application(s3AwsAccessKeyId));
      final String s3AwsSecretAccessKey = catalogApp.getToken();

      awsCredentials = new BasicAWSCredentials(s3AwsAccessKeyId, s3AwsSecretAccessKey);
    }

    return awsCredentials;
  }

  private S3Url parseS3ResourceUrl(final String url) {
    try {
      // The S3 Service uses links that follow the pattern http://endpoint/bucket/filename (internal
      // convention)
      final String acceptedUrlSchemes = configService.getConfigValue("sentilo.s3.url.accepted.schemes");
      final UrlValidator urlValidator = StringUtils.hasText(acceptedUrlSchemes) ? new UrlValidator(acceptedUrlSchemes.split(","))
          : new UrlValidator(UrlValidator.ALLOW_ALL_SCHEMES);
      if (!urlValidator.isValid(url)) {
        return new S3Url(false);
      }

      final URL parsedUrl = new URL(url);
      final String protocol = parsedUrl.getProtocol();
      final String authority = parsedUrl.getAuthority();
      final String path = parsedUrl.getPath();

      // Validate link format
      final String[] pathTokens = path.split("/");
      if (pathTokens.length == S3_PATH_TOKENS) {
        // Get first token: the bucket
        final String bucket = pathTokens[URL_BUCKET_POS];
        // Get second token: the filename
        final String filename = pathTokens[URL_FILENAME_POS];
        if (StringUtils.hasText(bucket) && StringUtils.hasText(filename)) {
          return new S3Url(protocol, authority, bucket, filename);
        }
      }

    } catch (final MalformedURLException e) {
      // Do nothing, the url was malformed!
    }

    // Incorrect S3 link format
    return new S3Url(false);
  }

}
