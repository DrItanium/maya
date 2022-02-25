/**
 * @file
 * Add SPI manipulation functionality
 * @copyright
 * maya
 * Copyright (c) 2012-2022, Joshua Scoggins
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

#include "electron/Environment.h"
#include <map>
#include <string>
#include <sys/ioctl.h>
#include <linux/types.h>
#include <linux/spi/spidev.h>
#include <memory>
#include <optional>
extern "C" {
#include <sys/stat.h>
#include <fcntl.h>
}

#include "SPIExtensions.h"
class SPIDevice {
public:
    using Self = SPIDevice;
    using Ptr = std::shared_ptr<Self>;
    enum class Mode {
        Zero = SPI_MODE_0,
        One = SPI_MODE_1,
        Two = SPI_MODE_2,
        Three = SPI_MODE_3,
    };
    enum class BitDirection : uint8_t {
        MSBFirst = 0,
        LSBFirst,
    };
    explicit SPIDevice(const std::string& path) : path_(path), fd_(::open(path.c_str(), O_RDWR)) {
    }
    ~SPIDevice() noexcept {
        if (fd_) {
           close(fd_);
        }
    }
private:
    template<typename T>
    [[nodiscard]] bool setViaIOCTL(unsigned long message, T value) noexcept {
           return ioctl(fd_, message, &value) >= 0;
    }
    template<typename R, typename T = R>
    [[nodiscard]] std::optional<R> getViaIOCTL(unsigned long message) const noexcept {
        if (T result{}; ioctl(fd_, message, &result) < 0) {
            return std::nullopt;
        } else {
            return std::make_optional(static_cast<R>(result));
        }
    }
public:
    bool setMode(Mode mode) noexcept { return setViaIOCTL(SPI_IOC_WR_MODE, static_cast<byte>(mode)); }
    bool setBitsPerWord(uint8_t numBits = 8) noexcept { return setViaIOCTL(SPI_IOC_WR_BITS_PER_WORD, numBits); }
    bool setDirection(BitDirection direction) noexcept { return setViaIOCTL(SPI_IOC_WR_LSB_FIRST, static_cast<byte>(direction)); }
    bool setMaxSpeed(uint32_t speed) noexcept { return setViaIOCTL(SPI_IOC_WR_MAX_SPEED_HZ, speed); }
    [[nodiscard]] std::optional<Mode> getMode() const noexcept { return getViaIOCTL<Mode, byte>(SPI_IOC_RD_MODE); }
    [[nodiscard]] std::optional<uint8_t> getNumBitsPerWord() const noexcept { return getViaIOCTL<uint8_t>(SPI_IOC_RD_BITS_PER_WORD); }
    [[nodiscard]] std::optional<BitDirection> getDirection() const noexcept { return getViaIOCTL<BitDirection, uint8_t>(SPI_IOC_RD_LSB_FIRST); }
    [[nodiscard]] std::optional<uint32_t> getMaxSpeed() const noexcept { return getViaIOCTL<uint32_t>(SPI_IOC_RD_MAX_SPEED_HZ); }
    [[nodiscard]] constexpr bool valid() const noexcept { return static_cast<bool>(fd_); }
    [[nodiscard]] constexpr const std::string& getPath() const noexcept { return path_; }
    int transfer(uint32_t speed, char* txBuf, char* rxBuf, unsigned int count, unsigned int delay = 0) noexcept {
        spi_ioc_transfer spi{};
        /// @todo implement
        spi.tx_buf = reinterpret_cast<decltype(spi.tx_buf)>(txBuf);
        spi.rx_buf = reinterpret_cast<decltype(spi.rx_buf)>(rxBuf);
        spi.len = count;
        spi.speed_hz = speed;
        if (auto result = getNumBitsPerWord(); result) {
            spi.bits_per_word = *result;
        } else {
            spi.bits_per_word = 8;
        }
        spi.cs_change = 0;
        return ioctl(fd_, SPI_IOC_MESSAGE(1), &spi);
    }
    int transfer(uint32_t speed, char* buf, unsigned int count, unsigned int delay = 0) noexcept {
        return transfer(speed, buf, buf, count, delay);
    }
private:
    std::string path_;
    int fd_;
};
namespace {
    std::map<std::string, SPIDevice::Ptr> openDeviceList_;
}
void
installSPIExtensions(Electron::Environment& theEnv) {
}
