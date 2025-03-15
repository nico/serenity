/*
 * Copyright (c) 2025, Nico Weber <thakis@chromium.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Coroutine.h>
#include <AK/Format.h>
#include <AK/Function.h>
#include <LibGfx/Rect.h>

namespace Gfx::JPEG2000 {

    template<typename T>
    struct Generator {
      struct promise_type;
      using handle_type = std::coroutine_handle<promise_type>;
    
      struct promise_type {
        T value_;
    
        Generator get_return_object() {
          return Generator(handle_type::from_promise(*this));
        }
        std::suspend_always initial_suspend() { return {}; }
        std::suspend_always final_suspend() noexcept { return {}; }
        void unhandled_exception() = delete;
        template<std::convertible_to<T> From>
        std::suspend_always yield_value(From &&from) {
          value_ = std::forward<From>(from);
          return {};
        }
        void return_void() {}

        void await_transform() = delete; // XXX??
      };
    
      handle_type h_;
    
      Generator(handle_type h) : h_(h) {}
      Generator(const Generator &) = delete;
      ~Generator() { h_.destroy(); }
      bool has_next() const {
        fill();
        return !h_.done();
      }
      T next() {
        fill();
        full_ = false;
        return std::move(h_.promise().value_);
      }
    
    private:
      mutable bool full_ = false;
    
      void fill() const {
        if (!full_) {
          h_();
          full_ = true;
        }
      }
    };

// B.12 Progression order
struct ProgressionData {
    int layer { 0 };
    int resolution_level { 0 };
    int component { 0 };
    int precinct { 0 };

    bool operator==(ProgressionData const&) const = default;
};

class ProgressionIterator {
public:
    virtual ~ProgressionIterator() = default;

    virtual bool has_next() const = 0;
    virtual ProgressionData next() = 0;
};

// B.12.1.1 Layer-resolution level-component-position progression
class LayerResolutionLevelComponentPositionProgressionIterator : public ProgressionIterator {
public:
    // FIXME: Supporting POC packets will probably require changes to this.
    LayerResolutionLevelComponentPositionProgressionIterator(int layer_count, int max_number_of_decomposition_levels, int component_count, Function<int(int resolution_level, int component)> precinct_count);
    virtual bool has_next() const override;
    virtual ProgressionData next() override;

private:
    Generator<ProgressionData> generator();

    int m_layer_count { 0 };
    int m_max_number_of_decomposition_levels { 0 };
    int m_component_count { 0 };
    Function<int(int resolution_level, int component)> m_precinct_count;
    Generator<ProgressionData> m_generator;
};

// B.12.1.2 Resolution level-layer-component-position progression
class ResolutionLevelLayerComponentPositionProgressionIterator : public ProgressionIterator {
public:
    // FIXME: Supporting POC packets will probably require changes to this.
    ResolutionLevelLayerComponentPositionProgressionIterator(int layer_count, int max_number_of_decomposition_levels, int component_count, Function<int(int resolution_level, int component)> precinct_count);
    virtual bool has_next() const override;
    virtual ProgressionData next() override;

private:
    Generator<ProgressionData> generator();

    int m_layer_count { 0 };
    int m_max_number_of_decomposition_levels { 0 };
    int m_component_count { 0 };
    Function<int(int resolution_level, int component)> m_precinct_count;
    Generator<ProgressionData> m_generator;
};

// B.12.1.3 Resolution level-position-component-layer progression
class ResolutionLevelPositionComponentLayerProgressionIterator : public ProgressionIterator {
public:
    // FIXME: Supporting POC packets will probably require changes to this.
    ResolutionLevelPositionComponentLayerProgressionIterator(int layer_count, int max_number_of_decomposition_levels, int component_count, Function<int(int resolution_level, int component)> precinct_count,
        Function<int(int component)> XRsiz, Function<int(int component)> YRsiz,
        Function<int(int resolution_level, int component)> PPx, Function<int(int resolution_level, int component)> PPy,
        Function<int(int component)> N_L,
        Function<int(int resolution_level, int component)> num_precincts_wide,
        Gfx::IntRect tile_rect,
        Function<IntRect(int resolution_level, int component)> ll_rect);
    virtual bool has_next() const override;
    virtual ProgressionData next() override;

private:
    Generator<ProgressionData> generator();

    int m_layer_count { 0 };
    int m_max_number_of_decomposition_levels { 0 };
    int m_component_count { 0 };
    Function<int(int resolution_level, int component)> m_precinct_count;
    Function<int(int component)> m_XRsiz;
    Function<int(int component)> m_YRsiz;
    Function<int(int resolution_level, int component)> m_PPx;
    Function<int(int resolution_level, int component)> m_PPy;
    Function<int(int component)> m_N_L;
    Function<int(int resolution_level, int component)> m_num_precincts_wide;
    Gfx::IntRect m_tile_rect;
    Function<IntRect(int resolution_level, int component)> m_ll_rect;
    Generator<ProgressionData> m_generator;
};

// B.12.1.4 Position-component-resolution level-layer progression
class PositionComponentResolutionLevelLayerProgressionIterator : public ProgressionIterator {
public:
    // FIXME: Supporting POC packets will probably require changes to this.
    PositionComponentResolutionLevelLayerProgressionIterator(int layer_count, int component_count, Function<int(int resolution_level, int component)> precinct_count,
        Function<int(int component)> XRsiz, Function<int(int component)> YRsiz,
        Function<int(int resolution_level, int component)> PPx, Function<int(int resolution_level, int component)> PPy,
        Function<int(int component)> N_L,
        Function<int(int resolution_level, int component)> num_precincts_wide,
        Gfx::IntRect tile_rect,
        Function<IntRect(int resolution_level, int component)> ll_rect);
    virtual bool has_next() const override;
    virtual ProgressionData next() override;

private:
    Generator<ProgressionData> generator();

    int m_layer_count { 0 };
    int m_component_count { 0 };
    Function<int(int resolution_level, int component)> m_precinct_count;
    Function<int(int component)> m_XRsiz;
    Function<int(int component)> m_YRsiz;
    Function<int(int resolution_level, int component)> m_PPx;
    Function<int(int resolution_level, int component)> m_PPy;
    Function<int(int component)> m_N_L;
    Function<int(int resolution_level, int component)> m_num_precincts_wide;
    Gfx::IntRect m_tile_rect;
    Function<IntRect(int resolution_level, int component)> m_ll_rect;
    Generator<ProgressionData> m_generator;
};

// B.12.1.5 Component-position-resolution level-layer progression
class ComponentPositionResolutionLevelLayerProgressionIterator : public ProgressionIterator {
public:
    // FIXME: Supporting POC packets will probably require changes to this.
    ComponentPositionResolutionLevelLayerProgressionIterator(int layer_count, int component_count, Function<int(int resolution_level, int component)> precinct_count,
        Function<int(int component)> XRsiz, Function<int(int component)> YRsiz,
        Function<int(int resolution_level, int component)> PPx, Function<int(int resolution_level, int component)> PPy,
        Function<int(int component)> N_L,
        Function<int(int resolution_level, int component)> num_precincts_wide,
        Gfx::IntRect tile_rect,
        Function<IntRect(int resolution_level, int component)> ll_rect);
    virtual bool has_next() const override;
    virtual ProgressionData next() override;

private:
    Generator<ProgressionData> generator();

    int m_layer_count { 0 };
    int m_component_count { 0 };
    Function<int(int resolution_level, int component)> m_precinct_count;
    Function<int(int component)> m_XRsiz;
    Function<int(int component)> m_YRsiz;
    Function<int(int resolution_level, int component)> m_PPx;
    Function<int(int resolution_level, int component)> m_PPy;
    Function<int(int component)> m_N_L;
    Function<int(int resolution_level, int component)> m_num_precincts_wide;
    Gfx::IntRect m_tile_rect;
    Function<IntRect(int resolution_level, int component)> m_ll_rect;
    Generator<ProgressionData> m_generator;
};

}

namespace AK {

template<>
struct Formatter<Gfx::JPEG2000::ProgressionData> : Formatter<FormatString> {
    ErrorOr<void> format(FormatBuilder& builder, Gfx::JPEG2000::ProgressionData const& value)
    {
        return Formatter<FormatString>::format(builder, "layer={}, resolution_level={}, component={}, precinct={}"sv, value.layer, value.resolution_level, value.component, value.precinct);
    }
};

}
