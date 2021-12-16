# delphi-android-SAF
Delphi Android Kapsamlı Depolama / Scoped Storage - Storage Access Framework SAF
Delphi ile Android 11 SDK 30 Kapsamlı Depolama
Scoped Storage : SAF & MediaStore API

Android 10 Gizlilik Değişiklikleri

Android 10 (API seviyesi 29), kullanıcıların gizliliğini daha iyi korumak için bir dizi özellik ve davranış değişikliği sunar. Bu değişiklikler, kullanıcıların verileri ve uygulamalara sağladıkları yetenekler üzerinde sahip oldukları şeffaflığı ve kontrolü genişletir. Bu özellikler, uygulamanızın bağlı olduğu belirli davranışların veya verilerin, platformun eski sürümlerine kıyasla farklı davranabileceği anlamına gelebilir. Uygulamanız, kullanıcı verilerini işlemek için mevcut en iyi uygulamaları izliyorsa, uygulamanız üzerindeki etkiler minimum düzeyde olmalıdır.

Uygulama dosyalarına ve medyaya yönelik harici depolama erişimi
Varsayılan olarak, Android 10 ve sonraki sürümleri hedefleyen uygulamalara harici depolamaya veya kapsamlı depolamaya kapsamlı erişim verilir. Bu tür uygulamalar, depolamayla ilgili herhangi bir kullanıcı izni istemeye gerek kalmadan harici bir depolama aygıtında aşağıdaki dosya türlerini görebilir:
· getExternalFilesDir (TPath.GetPublicPath) kullanılarak erişilen uygulamaya özel dizindeki dosyalar.
· Uygulamanın medya mağazasında oluşturduğu fotoğraflar, videolar ve ses klipleri.
Kapsamlı depolama ile harici depolama cihazlarına kaydedilmiş dosyaları paylaşma, ulaşma ve değiştirme hakkında daha fazla bilgi için harici depolama dosyaları yönetme ve medya dosyalarını ulaşma ve değiştirme kılavuzlarına bakın.
https://developer.android.com/about/versions/10/privacy/changes

Android 11 (API 30) Gizlilik ve Güvenlik
Android 11'i hedefleyen uygulamaların, yalnızca kendi oluşturdukları harici depolama alanındaki dosyalara ("scoped storage") erişmeye izni vardır. Ayrıca uygulamaya özel bir dizinde bulunan dosyalara ek olarak; ana dizindeki "Müzik", "Resimler" veya "Video" dizinleri erişime dahildir. Diğer herhangi bir dosyaya ancak ve ancak "Depolama Erişim Çerçevesi / Storage Access Framework" aracılığıyla ve kullanıcı izni yoluyla erişilebilir.
Uygulamada belirlenen konum izni sayesinde üretilen EXIF konum verisinin başarıyla işlenmiş olmasından emin olmak için Android 11, video ve fotoğraf kayıt "niyetlerini / intents" yalnızca sistem kamera uygulamasına olmak üzere sınırlar.
https://tr.wikipedia.org/wiki/Android_11

Android 11, kullanıcı gizliliğini geliştirmek için aşağıdakiler dahil olmak üzere değişiklikler ve kısıtlamalar getirmiştir:
Kapsamlı depolama mecburiyeti : Harici depolama dizinlerine erişim, uygulamaya özel bir dizin ve uygulamanın oluşturduğu belirli ortam türleriyle sınırlıdır.
İzinlerin otomatik olarak sıfırlanması : Kullanıcılar bir uygulamayla birkaç aydır etkileşimde bulunmadıysa, sistem uygulamanın hassas izinlerini otomatik olarak sıfırlar.
Arka planda konum erişimi : Uygulamalara arka planda konum izni vermek için kullanıcıların sistem ayarlarına yönlendirilmesi gerekir.
Paket görünürlüğü : Bir uygulama, cihazda yüklü uygulamaların listesini sorguladığında, döndürülen liste filtrelenir.
https://developer.android.com/about/versions/11/behavior-changes-all

    Kapsamlı depolama sınırlaması
    Arka planda konum erişimi ve resim, video ve ses dosyalarına ulaşma için yeni izinler

Google Play, Tüm dosyalara erişim adlı özel uygulama erişimi de dahil olmak üzere yüksek riskli ya da hassas izinlerin kullanımını kısıtlıyor. Bu, yalnızca Android 11'i hedefleyen (API düzeyi 30) uygulamalar ve Android 11'de eklenen MANAGE_EXTERNAL_STORAGE iznini beyan eden uygulamalar için geçerlidir. Ayrıca bu politika, READ_EXTERNAL_STORAGE izninin kullanımını etkilemez.
Uygulamanız MANAGE_EXTERNAL_STORAGE iznine erişilmesini gerektirmiyorsa uygulamanızı başarılı bir şekilde yayınlayabilmek için bu izni uygulamanızın manifest dosyasından kaldırmanız gerekir. Politikaya uygun alternatiflerin uygulanması hakkında ayrıntılı bilgiyi aşağıda bulabilirsiniz.
Uygulamanız kabul edilebilir kullanım ile ilgili politika gereksinimlerini karşılıyorsa veya istisna olarak tutulmaya uygunsa Play Console'daki Beyan Formu'nu kullanarak bunu ve diğer yüksek riskli izinleri bildirmeniz gerekir.
Politika gereksinimlerini karşılamayan veya Beyan Formu gönderilmeyen uygulamalar Google Play'den kaldırılabilir.
Tüm dosyalara erişim iznine yalnızca uygulamanız, gizliliği daha fazla koruyan en iyi uygulamaları verimli bir şekilde kullanamadığında (ör. Depolama Erişim Çerçevesi veya Media Store API kullanımı) erişmelisiniz.
Buna ek olarak, uygulamanın izin kullanımı, izin verilen kullanımlar kapsamına girmeli ve uygulamanın temelişleviyle doğrudan bağlantılı olmalıdır. Temel işlev, uygulamanın asıl amacı olarak tanımlanır. Bu temel işlev olmadan uygulama "çalışmaz" veya kullanışlı olmaz. Temel işlevin yanı sıra bu temel işlevi oluşturan tüm temel özellikler, uygulamanın açıklamasında belirgin bir şekilde belgelenmeli ve tanıtılmalıdır.
https://support.google.com/googleplay/android-developer/answer/10467955?hl=tr

Paylaşılan depolamaya genel bakış

Diğer uygulamalar tarafından erişilebilen veya erişilmesi gereken ve kullanıcı uygulamanızın yüklemesini kaldırsa bile kaydedilen kullanıcı verileri için paylaşılan depolamayı kullanır.
Android, paylaşılabilir veri türlerini depolamak ve bunlara erişmek için aşağıdaki API'ler sağlar:

    Medya içeriği: Sistem, bu tür dosyalar için standart genel dizinler sağlar, böylece kullanıcının tüm fotoğrafları için ortak bir konumu, tüm müzik ve ses dosyaları için başka bir ortak konumu vb. vardır. Uygulamanız, platformu MediaStore API'sini kullanarak bu içeriğe erişebilir.
    Belgeler ve diğer dosyalar: Sistem, PDF belgeleri ve EPUB biçimini kullanan kitaplar gibi diğer dosya türlerini içeren özel bir dizine sahiptir. Uygulamanız, platformun Depolama Erişim Çerçevesini (SAF) kullanarak bu dosyalara erişebilir.
    Veritabanları : Android 11 (API düzeyi 30) ve sonraki sürümlerde sistem, birden fazla uygulamanın kullanabileceği büyük Veritabanlarıni önbelleğe alır. Bu Veritabanları, makine öğrenimi ve medya oynatma gibi kullanma vakalarını destekleyebilir. Uygulamalar, BlobStoreManager API'yi kullanarak bu paylaşılan Veritabanlarıne erişebilir.
    Bu API'ler hakkında daha fazla bilgi için aşağıdaki kılavuzlara bakınız:
    Medya içeriği
    Belgeler ve diğer dosyalar
    Veritabanları
    https://developer.android.com/training/data-storage/shared

Kapsamlı Depolama / Scoped Storage

Android 10 ve 11 gizlilik ve güvenlik değişikliklerinden sonra harici depolamaya erişim kapsamlı hale getirilmiş olup, artık dosyalara ulaşmanın 4 ana yolu mevcuttur:

1. Depolama Erişim Çerçevesi / Storage Access Framework (SAF)
2. MediaStore API
3. All Files Access API
4. Paylaşmalı Veritabanları API

All Files Access API (ACTION_MANAGE_APP_ALL_FILES_ACCESS_PERMISSION izni ve ACTION_MANAGE_STORAGE niyeti) sadece dosya yöneticisi ve antivirus türü uygulamalar için çıkarılmış olup, diğer tüm uygulamalar Google Play Store tarafından reddedilmektedir.
Paylaşmalı Veritabanları / Databases (BlobStoreManager) API de sadece Android 11 SDK 30 ve üstünde çalışmakta olup, Android 10 ve önceki sürümlerde çalışmamaktadır.
Dolayısıyla bu iki API konuya dahil edilmemiştir.
Depolama Erişim Çerçevesi / Storage Access Framework (SAF)

Android 4.4 (API seviyesi 19), Storage Access Framework (SAF) sınıfını sunmuştur. SAF, kullanıcıların tercih ettikleri tüm belge depolama sağlayıcılarında belgelere, resimlere ve diğer dosyalara göz atmasını ve bunları açmasını kolaylaştırır. Standart, kullanımı kolay bir kullanıcı arayüzü, kullanıcıların dosyalara göz atmasına ve uygulamalar ve sağlayıcılar arasında tutarlı bir şekilde son bilgilere erişmesine olanak tanır.
Bulut veya yerel depolama hizmetleri, DocumentsProvider hizmetlerini kapsayan bir uygulama uygulayarak bu ekosisteme katılabilir. Bir sağlayıcının belgelerine erişmesi gereken istemci uygulamaları, yalnızca birkaç satır kodla SAF ile entegre olabilir.
SAF aşağıdakileri ihtiva eder:

Belge sağlayıcı —Bir depolama hizmetinin (Google Drive gibi) yönettiği dosyaları ortaya çıkarmasına izin veren bir içerik sağlayıcı. Bir belge sağlayıcı, sınıfın bir alt DocumentsProvider sınıfı olarak uygulanır. Belge sağlayıcı şeması, geleneksel bir dosya hiyerarşisine dayanır, ancak belge sağlayıcınızın verileri fiziksel olarak nasıl depolayacağı size bağlıdır. Android platformu, İndirilenler, Görüntüler ve Videolar gibi çeşitli yerleşik belge sağlayıcıları içerir.
İstemci uygulaması Her bir özel uygulama vakaları olan ACTION_CREATE_DOCUMENT, ACTION_OPEN_DOCUMENT ve ACTION_OPEN_DOCUMENT_TREE niyet eylemlerini (intent action) çağırır ve belge sağlayıcıları tarafından döndürülen dosyaları alır.
Seçici —Kullanıcıların, istemci uygulamasının arama kriterlerini karşılayan tüm belge sağlayıcılarından belgelere erişmesine olanak tanıyan bir sistem kullanıcı arabirimi.

SAF'ın sunduğu özelliklerden bazıları şunlardır:

Kullanıcıların yalnızca tek bir uygulamadan değil, tüm belge sağlayıcılardan gelen içeriğe göz atmasına olanak tanır.
Uygulamanızın, bir belge sağlayıcıya ait belgelere uzun vadeli, kalıcı erişime sahip olmasını mümkün kılar. Bu erişim sayesinde kullanıcılar sağlayıcıya dosya ekleyebilir, düzenleyebilir, kaydedebilir ve silebilir.
Yalnızca sürücü takılıyken görünen USB depolama sağlayıcıları gibi birden çok kullanıcı hesabını ve geçici kökleri destekler.

Genel bakış
SAF, DocumentsProvider sınıfın bir alt sınıfı olan bir içerik sağlayıcı etrafında toplanır. Bir belge sağlayıcı içinde veriler, geleneksel bir dosya hiyerarşisi olarak yapılandırılmıştır:

storage_datamodel.png
Şekil 1. Belge sağlayıcı veri modeli. Bir Kök, tek bir Belgeye işaret eder ve ardından tüm ağacın yayılmasını başlatır.
Aşağıdakilere dikkat ediniz:

Her belge sağlayıcı, bir belge ağacını keşfetmeye başlama noktaları olan bir veya daha fazla 'kök' bildirir. Her kökün benzersiz bir değeri vardır COLUMN_ROOT_ID ve bu kökün altındaki içeriği temsil eden bir belgeye (bir dizine) işaret eder. Kökler, birden çok hesap, geçici USB depolama aygıtı veya kullanıcı oturum açma/oturum kapatma gibi kullanım durumlarını desteklemek için tasarım gereği dinamiktir.

Her kökün altında tek bir belge bulunur. Bu belge 1'den N'ye kadar belgeye işaret eder ve bunların her biri sırayla 1'den N'ye kadar belgeye işaret edebilir.

Her depolama arka ucu, benzersiz bir COLUMN_DOCUMENT_ID. Cihaz yeniden başlatmaları arasında kalıcı URI izinleri için kullanıldığından, belge kimlikleri benzersiz olmalı ve yayınlandıktan sonra değişmemelidir.

Belgeler, açılabilir bir dosya (belirli bir MIME türüyle) veya ek belgeler içeren bir dizin ( MIME_TYPE_DIR MIME türüyle) olabilir.

Her belge, COLUMN_FLAGS tarafından açıklandığı gibi farklı yeteneklere sahip olabilir. Örneğin FLAG_SUPPORTS_WRITE, FLAG_SUPPORTS_DELETE ve FLAG_SUPPORTS_THUMBNAIL. Aynı COLUMN_DOCUMENT_ID çoklu dizinleri dahil edilebilir.

Kontrol akışı
Yukarıda belirtildiği gibi, belge sağlayıcı veri modeli, geleneksel bir dosya hiyerarşisine dayanmaktadır. Ancak DocumentsProvider API kullanarak erişebildiğiniz sürece verilerinizi fiziksel olarak istediğiniz gibi saklayabilirsiniz. Örneğin, verileriniz için etiket tabanlı bulut depolamayı kullanabilirsiniz.
Şekil 2, bir fotoğraf uygulamasının depolanan verilere erişmek için SAF'ı nasıl kullanabileceğini gösterir:

storage_dataflow.png
Şekil2. Depolama Erişim Çerçevesi akış şeması
Aşağıdakilere dikkat ediniz:

SAF'ta sağlayıcılar ve müşteriler doğrudan etkileşime girmez. İstemci, dosyalarla etkileşim (yani, dosyaları okumak, düzenlemek, oluşturmak veya silmek) için izin ister.

Bir uygulamanın (bu örnekte, bir fotoğraf uygulaması) ACTION_OPEN_DOCUMENT veya ACTION_CREATE_DOCUMENT niyeti etkileşimi. Niyet /intent şartları daha da hassaslaştırmak için filtreler içerebilir; örneğin, "image” MIME türüne sahip tüm açılabilir dosyaları verir.

Niyet harekete geçtiğinde, sistem seçici her kayıtlı sağlayıcıya gider ve kullanıcıya eşleşen içerik köklerini gösterir.

Seçici, temeldeki belge sağlayıcıları çok farklı olsa bile, kullanıcılara belgelere erişmek için standart bir arabirim sağlar. Örneğin, şekil 2 bir Google Drive sağlayıcısını, bir USB sağlayıcısını ve bir bulut sağlayıcısını göstermektedir.

Şekil 3, görüntüleri arayan bir kullanıcının İndirilenler klasörünü seçtiği bir seçiciyi göstermektedir. Ayrıca, istemci uygulamasının kullanabileceği tüm kökleri de gösterir.

storage_picker.svg
Şekil 3. (Sistem Dosya) Seçici
Kullanıcı İndirilenler klasörünü seçtikten sonra resimler görüntülenir. Şekil 4, bu işlemin sonucunu göstermektedir. Kullanıcı artık bu görüntülerle sağlayıcının ve istemci uygulamasının desteklediği şekillerde etkileşim kurabilir.

storage_photos.svg
Şekil 4. Sistem seçicide görüntülendiği şekliyle İndirilenler klasöründe saklanan resimler

İstemci uygulaması yazma
Android 4.3 ve önceki sürümlerde, uygulamanızın başka bir uygulamadan dosya almasını istiyorsanız, ACTION_PICK veya ACTION_GET_CONTENT gibi bir niyeti çağırılması gerekir. Kullanıcı daha sonra içinden bir dosya seçeceği tek bir uygulama seçmeli ve seçilen uygulama, kullanıcının mevcut dosyalara göz atması ve aralarından seçim yapması için bir kullanıcı arabirimi sağlamalıdır.
Android 4.4 (API düzeyi 19) ve sonraki sürümlerde, ACTION_OPEN_DOCUMENT kullanıcının diğer uygulamaların kullanıma sunduğu tüm dosyalara göz atmasına olanak tanıyan, sistem kontrollü bir seçici kullanıcı arabirimini görüntüleyen niyeti kullanma ek seçeneğiniz vardır. Bu tek kullanıcı arayüzünden kullanıcı, desteklenen uygulamalardan herhangi birinden bir dosya seçebilir.
Android 5.0 (API düzeyi 21) ve sonraki sürümlerde, kullanıcının bir istemci uygulamasının erişmesi için bir dizin seçmesine olanak tanıyan ACTION_OPEN_DOCUMENT_TREE niyetini de kullanabilirsiniz.
Not: ACTION_OPEN_DOCUMENT niyetinin ACTION_GET_CONTENT‘in yerine geçmesi amaçlanmamıştır. Kullanma Vakası uygulamanızın ihtiyaçlarına bağlıdır:

ACTION_GET_CONTENT Uygulamanızın yalnızca verileri okumasını veya içe aktarmasını istiyorsanız kullanın. Bu yaklaşımla uygulama, resim dosyası gibi verilerin bir kopyasını içe aktarır.

ACTION_OPEN_DOCUMENT Uygulamanızın bir belge sağlayıcıya ait belgelere uzun süreli, kalıcı erişime sahip olmasını istiyorsanız kullanın. Bir örnek, kullanıcıların bir belge sağlayıcıda depolanan görüntüleri düzenlemesine izin veren bir fotoğraf düzenleme uygulaması olabilir.

Sistem seçici kullanıcı arabirimini kullanarak dosya ve dizinlere göz atmayı nasıl destekleyeceğiniz hakkında daha fazla bilgi için belgelere ve diğer dosyalara nasıl erişileceğine dair kılavuza bakın.

Yazının devamı için bakınız:
https://www.delphican.com/showthread.php?tid=6470
